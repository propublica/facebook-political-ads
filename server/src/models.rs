use chrono::DateTime;
use chrono::offset::Utc;
use diesel;
use diesel::prelude::*;
use diesel::pg::PgConnection;
use diesel::pg::upsert::*;
use futures::future;
use futures::future::Executor;
use futures::stream;
use futures::{Stream, Future};
use futures_cpupool::CpuPool;
use hyper::{Client, Body, Uri};
use hyper::client::HttpConnector;
use hyper::error::UriError;
use hyper_tls::HttpsConnector;
use InsertError;
use kuchiki;
use kuchiki::traits::*;
use r2d2_diesel::ConnectionManager;
use r2d2::Pool;
use rusoto_core::{Region, default_tls_client};
use rusoto_credential::DefaultCredentialsProvider;
use rusoto_s3::S3Client;
use schema::ads;
use server::AdPost;
use tokio_core::reactor::Core;



#[derive(Queryable, Debug)]
pub struct Ad {
    id: String,
    html: String,
    political: i32,
    not_political: i32,

    fuzzy_id: Option<i32>,
    title: String,
    message: String,
    thumbnail: String,

    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,

    browser_lang: String,

    images: Vec<String>,
}

impl Ad {
    // This will asynchronously save the images to s3 we may very well end up
    // with broken images. but I can't see any way around it right now.  Also we
    // should think about splitting this up, but I'm fine -- if a little
    // embarassed about it right now.
    pub fn grab_and_store(
        &self,
        core: &mut Core,
        db: &Pool<ConnectionManager<PgConnection>>,
        pool: CpuPool,
    ) {
        let images = [vec![self.thumbnail.clone()], self.images.clone()]
            .concat()
            .iter()
            .map(|a| {
                let a: Uri = a.parse().map_err(InsertError::Uri)?;
                Ok(a.clone())
            })
            .collect::<Vec<Result<Uri, InsertError>>>();
        let ad = self.clone();
        let pool_s3 = pool.clone();
        let pool_db = pool.clone();
        let db = db.clone();
        let client = Client::new(&core.handle());
        let future = stream::iter(images)
            // grab the images
            .map(move |img| {
                let cloned = img.clone();
                client
                    .get(img)
                    .and_then(|res| {
                        res.body().concat2().and_then(|chunk| Ok((chunk, cloned)))
                    })
                    .map_err(InsertError::Hyper)
            })
            // upload them to s3
            .and_then(move |future| {
                let pool = pool_s3.clone();
                future.and_then(move |tuple| {
                    pool.spawn_fn(move || {
                        let client = S3Client::new(default_tls_client().map_err(InsertError::TLS)?,
                                                   DefaultCredentialsProvider::new().unwrap(),
                                                   Region::UsEast1);
                        
                        Ok(tuple.1)
                    })
                })
            })
            .collect()
            // save the new urls to the database the images variable will
            // include only those that we've successfully saved, so we have to
            // do a funky merge here.
            .and_then(move |images| {
                pool_db.spawn_fn(|| Ok(()))
            }).map_err(|e| {
                warn!("{:?}", e);
                ()
            });
        let result = core.execute(future);
        if result.is_err() {
            warn!("Tokio Core error: {:?}", result);
        }
    }
}

#[derive(Insertable)]
#[table_name = "ads"]
pub struct NewAd<'a> {
    id: &'a str,
    html: &'a str,
    political: i32,
    not_political: i32,

    title: String,
    message: String,
    thumbnail: String,

    browser_lang: &'a str,
    images: Vec<String>,
}

#[derive(AsChangeset)]
#[table_name = "ads"]
pub struct Images {
    thumbnail: String,
    images: Vec<String>,
}

impl<'a> NewAd<'a> {
    fn get_title(document: &kuchiki::NodeRef) -> Result<String, InsertError> {
        document
            .select("h5 a, h6 a, strong")
            .map_err(InsertError::HTML)?
            .nth(0)
            .and_then(|a| Some(a.text_contents()))
            .ok_or(InsertError::HTML(()))
    }

    fn get_image(document: &kuchiki::NodeRef) -> Result<String, InsertError> {
        document
            .select("img")
            .map_err(InsertError::HTML)?
            .nth(0)
            .and_then(|a| {
                a.attributes.borrow().get("src").and_then(
                    |src| Some(src.to_string()),
                )
            })
            .ok_or(InsertError::HTML(()))
    }
    // Video ads make this a bit messy
    fn get_message(document: &kuchiki::NodeRef) -> Result<String, InsertError> {
        let selectors = vec![".userContent p", "span"];
        let iters = selectors.iter().map(|s| document.select(s)).flat_map(|a| a);

        iters
            .map(|i| {
                i.fold(String::new(), |m, a| m + &a.as_node().to_string())
            })
            .filter(|i| i.len() > 0)
            .nth(0)
            .ok_or(InsertError::HTML(()))
    }

    fn get_images(document: &kuchiki::NodeRef) -> Result<Vec<String>, InsertError> {
        Ok(
            document
                .select("img")
                .map_err(InsertError::HTML)?
                .skip(1)
                .map(|a| {
                    a.attributes.borrow().get("src").and_then(
                        |s| if !s.contains(
                            "rsrc.php",
                        )
                        {
                            Some(s.to_string())
                        } else {
                            None
                        },
                    )
                })
                .filter(|s| s.is_some())
                .map(|s| s.unwrap())
                .collect::<Vec<String>>(),
        )
    }

    pub fn new(ad: &'a AdPost) -> Result<NewAd<'a>, InsertError> {
        let document = kuchiki::parse_html().one(ad.html.clone());

        let message = NewAd::get_message(&document)?;
        let title = NewAd::get_title(&document)?;
        let thumb = NewAd::get_image(&document)?;
        let images = NewAd::get_images(&document)?;

        Ok(NewAd {
            id: &ad.id,
            html: &ad.html,
            political: if ad.political { 1 } else { 0 },
            not_political: if !ad.political { 1 } else { 0 },
            title: title,
            message: message,
            thumbnail: thumb,
            browser_lang: &ad.browser_lang,
            images: images,
        })
    }

    pub fn save(&self, pool: &Pool<ConnectionManager<PgConnection>>) -> Result<Ad, InsertError> {
        use schema::ads;
        use schema::ads::dsl::*;
        let connection = pool.get().map_err(InsertError::Timeout)?;
        let ad: Ad = diesel::insert(&self.on_conflict(
            id,
            do_update().set((
                political.eq(political + self.political),
                not_political.eq(
                    not_political +
                        self.not_political,
                ),
                updated_at.eq(Utc::now()),
            )),
        )).into(ads::table)
            .get_result(&*connection)
            .map_err(InsertError::DataBase)?;
        Ok(ad)
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ad_parsing() {
        let ad = include_str!("./html-test.txt");
        let post = AdPost {
            id: "test".to_string(),
            html: ad.to_string(),
            political: true,
            browser_lang: "en-US".to_string(),
        };
        let new_ad = NewAd::new(&post).unwrap();
        assert!(new_ad.thumbnail.len() > 0);
        assert_eq!(new_ad.images.len(), 1);
        assert_eq!(
            new_ad.message,
            "<p><a href=\"https://www.facebook.com/hashtag/valerian\" class=\"_58cn\"><span class=\"_5afx\"><span class=\"_58cl _5afz\">#</span><span class=\"_58cm\">Valerian</span></span></a> is “the best experience since ‘Avatar.’” See it in 3D and RealD3D theaters this Friday. Get tickets now: <a>ValerianTickets.com</a></p>"
        );
        assert!(new_ad.title.len() > 0);
    }
}
