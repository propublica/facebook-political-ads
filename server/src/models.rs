use chrono::DateTime;
use chrono::offset::Utc;
use diesel;
use diesel::prelude::*;
use diesel::pg::PgConnection;
use diesel::pg::upsert::*;
use futures::{Future, stream, Stream};
use futures_cpupool::CpuPool;
use hyper::{Body, Client, Uri};
use hyper::client::HttpConnector;
use hyper_tls::HttpsConnector;
use InsertError;
use kuchiki;
use kuchiki::traits::*;
use r2d2_diesel::ConnectionManager;
use r2d2::Pool;
use rusoto_core::{default_tls_client, Region};
use rusoto_credential::DefaultCredentialsProvider;
use rusoto_s3::{PutObjectRequest, S3Client, S3};
use schema::ads;
use server::AdPost;

const ENDPOINT: &'static str = "https://pp-facebook-ads.s3.amazonaws.com/";

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

fn get_message(document: &kuchiki::NodeRef) -> Result<String, InsertError> {
    let selectors = vec![".userContent p", "span"];
    let iters = selectors.iter().map(|s| document.select(s)).flat_map(|a| a);

    iters
        .map(|i| {
            i.fold(String::new(), |m, a| m + &a.as_node().to_string())
        })
        .filter(|i| !i.is_empty())
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
                    |s| Some(s.to_string()),
                )
            })
            .filter(|s| s.is_some())
            .map(|s| s.unwrap())
            .collect::<Vec<String>>(),
    )
}

#[derive(AsChangeset, Debug)]
#[table_name = "ads"]
pub struct Images {
    thumbnail: Option<String>,
    images: Vec<String>,
    title: String,
    message: String,
    html: String,
}

impl Images {
    fn from_ad(ad: &Ad, images: Vec<Uri>) -> Result<Images, InsertError> {
        let thumb = images
            .iter()
            .filter(|i| ad.thumbnail.contains(i.path()))
            .map(|i| ENDPOINT.to_string() + i.path().trim_left_matches('/'))
            .nth(0);

        let mut rest = images.clone();
        if let Some(thumb) = thumb.clone() {
            rest.retain(|x| !thumb.contains(x.path()))
        };

        let collection = rest.iter()
            .filter(|i| ad.images.iter().any(|a| a.contains(i.path())))
            .map(|i| ENDPOINT.to_string() + i.path().trim_left_matches('/'))
            .collect::<Vec<String>>();

        let document = kuchiki::parse_html().one(ad.html.clone());
        for a in document.select("img").map_err(InsertError::HTML)? {
            if let Some(x) = a.attributes.borrow_mut().get_mut("src") {
                if let Ok(u) = x.parse::<Uri>() {
                    if let Some(i) = images.iter().find(|i| i.path() == u.path()) {
                        *x = ENDPOINT.to_string() + i.path().trim_left_matches('/');
                    } else {
                        *x = "".to_string();
                    }
                }
            };
        }

        let title = get_title(&document)?;
        let message = get_title(&document)?;
        Ok(Images {
            thumbnail: thumb,
            images: collection,
            title: title,
            html: document
                .select("div")
                .map_err(InsertError::HTML)?
                .nth(0)
                .ok_or(InsertError::HTML(()))?
                .as_node()
                .to_string(),
            message: message,
        })
    }
}

#[derive(Queryable, Debug, Clone)]
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
    impressions: i32,
}

impl Ad {
    // This will asynchronously save the images to s3 we may very well end up
    // dropping images, but I can't see any way around it right now. Also we
    // should think about splitting this up, but I'm fine -- if a little
    // embarassed about it -- right now. This function swallows errors, and
    // there's a chance we'll end up with no images at the end, but I think we
    // can handle that in the extension's UI.
    pub fn grab_and_store(
        &self,
        client: Client<HttpsConnector<HttpConnector>, Body>,
        db: &Pool<ConnectionManager<PgConnection>>,
        pool: CpuPool,
    ) -> Box<Future<Item = (), Error = ()>> {
        let ad = self.clone();
        let pool_s3 = pool.clone();
        let pool_db = pool.clone();
        let db = db.clone();
        let future = stream::iter(self.image_urls())
            // filter ones we already have in the db and ones we can verify as
            // coming from fb, we don't want to become a malware vector :)
            .filter(|u| {
                match u.host() {
                    Some(h) => !(h == "pp-facebook-ads.s3.amazonaws.com" || h.ends_with("fbcdn.net")),
                    None => false
                }
            })
            // grab image
            .and_then(move |img| {
                info!("getting {:?}", img);
                let cloned = img.clone();
                client
                    .get(img)
                    .and_then(|res| {
                        res.body().concat2().and_then(|chunk| Ok((chunk, cloned)))
                    })
                    .map_err(InsertError::Hyper)
            })
            // upload them to s3
            .and_then(move |tuple| {
                let pool = pool_s3.clone();
                // we do this in a worker thread because rusoto isn't on
                // Hyper async yet.
                pool.spawn_fn(move || {
                    let client =
                        S3Client::new(default_tls_client().map_err(InsertError::TLS)?,
                                      DefaultCredentialsProvider::new().map_err(InsertError::AWS)?,
                                      Region::UsEast1);
                    let req = PutObjectRequest {
                        bucket: "pp-facebook-ads".to_string(),
                        key: tuple.1.path().trim_left_matches('/').to_string(),
                        acl: Some("public-read".to_string()),
                        body: Some(tuple.0.to_vec()),
                        ..PutObjectRequest::default()
                    };
                    client.put_object(&req).map_err(InsertError::S3)?;
                    Ok(tuple.1)
                })
            })
            .map_err(|e| {
                warn!("{:?}", e);
                ()
            })
            .collect()
            // save the new urls to the database. the images variable will
            // include only those that we've successfully saved to s3, so we
            // have to do a funky merge here.
            .and_then(move |images| {
                let imgs = images.clone();
                pool_db.spawn_fn(move || {
                    use schema::ads::dsl::*;
                    // I don't understand why we're zeroing out the errors here
                    // but ok.
                    let update = Images::from_ad(&ad, imgs).map_err(|e| {warn!("{:?}", e); ()})?;
                    let connection = db.get().map_err(|e| {warn!("{:?}", e); ()})?;
                    diesel::update(ads.filter(id.eq(&ad.id)))
                        .set(&update)
                        .execute(&*connection)
                        .map_err(|e| {warn!("{:?}", e); ()})?;
                    info!("saved {:?}", ad.id);
                    Ok(())
                })
            });
        Box::new(future)
    }

    pub fn image_urls(&self) -> Vec<Result<Uri, InsertError>> {
        let images = [vec![self.thumbnail.clone()], self.images.clone()];
        images
            .concat()
            .iter()
            .map(|a| {
                let a: Uri = a.parse().map_err(InsertError::Uri)?;
                Ok(a)
            })
            .collect()
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
    impressions: i32,
}

impl<'a> NewAd<'a> {
    pub fn new(ad: &'a AdPost) -> Result<NewAd<'a>, InsertError> {
        let document = kuchiki::parse_html().one(ad.html.clone());

        let thumb = get_image(&document)?;
        let images = get_images(&document)?;
        let message = get_message(&document)?;
        let title = get_title(&document)?;

        Ok(NewAd {
            id: &ad.id,
            html: &ad.html,
            political: if ad.political.unwrap_or(false) { 1 } else { 0 },
            not_political: if !ad.political.unwrap_or(true) { 1 } else { 0 },
            title: title,
            message: message,
            thumbnail: thumb,
            browser_lang: &ad.browser_lang,
            images: images,
            impressions: if !ad.political.is_some() { 1 } else { 0 },
        })
    }

    pub fn save(&self, pool: &Pool<ConnectionManager<PgConnection>>) -> Result<Ad, InsertError> {
        use schema::ads;
        use schema::ads::dsl::*;
        let connection = pool.get().map_err(InsertError::Timeout)?;
        // increment impressions if this is a background save
        let ad: Ad = diesel::insert(&self.on_conflict(
            id,
            do_update().set((
                political.eq(political + self.political),
                not_political.eq(
                    not_political +
                        self.not_political,
                ),
                impressions.eq(
                    impressions + self.impressions,
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
            political: None,
            browser_lang: "en-US".to_string(),
        };
        let new_ad = NewAd::new(&post).unwrap();
        assert!(new_ad.thumbnail.len() > 0);
        assert_eq!(new_ad.images.len(), 2);
        assert!(new_ad.title.len() > 0);

        assert_eq!(
            kuchiki::parse_html().one(new_ad.message).text_contents(),
            kuchiki::parse_html().one("<p><a class=\"_58cn\" href=\"https://www.facebook.com/hashtag/valerian\"><span class=\"_5afx\"><span class=\"_58cl _5afz\">#</span><span class=\"_58cm\">Valerian</span></span></a> is “the best experience since ‘Avatar.’” See it in 3D and RealD3D theaters this Friday. Get tickets now: <a>ValerianTickets.com</a></p>").text_contents()
        );
    }

    #[test]
    fn image_parsing() {
        use chrono::prelude::*;
        let ad = include_str!("./html-test.txt");
        let document = kuchiki::parse_html().one(ad);

        let saved_ad = Ad {
            id: "test".to_string(),
            html: ad.to_string(),
            political: 1,
            not_political: 2,
            fuzzy_id: None,
            title: get_title(&document).unwrap(),
            message: get_message(&document).unwrap(),
            thumbnail: get_image(&document).unwrap(),
            created_at: Utc::now(),
            updated_at: Utc::now(),
            browser_lang: "en-US".to_string(),
            images: get_images(&document).unwrap(),
            impressions: 1,
        };
        let urls = saved_ad
            .image_urls()
            .into_iter()
            .map(|x| x.unwrap())
            .collect();
        let images = Images::from_ad(&saved_ad, urls).unwrap();
        assert!(images.html != saved_ad.html);
        assert!(!images.html.contains("fbcdn"));
        assert!(!images.html.contains("html"));
        assert!(images.images.len() == saved_ad.images.len());
    }
}
