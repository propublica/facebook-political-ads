use chrono::DateTime;
use chrono::offset::Utc;
use diesel;
use diesel::prelude::*;
use diesel::pg::PgConnection;
use diesel::pg::upsert::*;
use r2d2_diesel::ConnectionManager;
use r2d2::Pool;
use super::InsertError;
use super::schema::ads;
use super::server::AdPost;
use kuchiki;
use kuchiki::traits::*;


#[derive(Queryable, Debug)]
pub struct Ad {
    id: String,
    html: String,
    political: i32,
    not_political: i32,

    fuzzy_id: Option<i32>,
    title: Option<String>,
    message: Option<String>,
    thumbnail: Option<String>,

    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,

    browser_lang: String,

    images: Option<Vec<String>>,
}

#[derive(Insertable)]
#[table_name = "ads"]
pub struct NewAd<'a> {
    id: &'a str,
    html: &'a str,
    political: i32,
    not_political: i32,

    title: Option<String>,
    message: Option<String>,
    thumbnail: Option<String>,

    browser_lang: &'a str,
    images: Option<Vec<String>>,
}

#[derive(AsChangeset)]
#[table_name = "ads"]
pub struct Images {
    thumbnail: Option<String>,
    images: Option<Vec<String>>,
}

impl<'a> NewAd<'a> {
    fn get_title(document: &kuchiki::NodeRef) -> Result<Option<String>, InsertError> {
        Ok(
            document
                .select("h5 a, h6 a, strong")
                .map_err(InsertError::HTML)?
                .nth(0)
                .and_then(|a| Some(a.text_contents())),
        )
    }

    fn get_image(document: &kuchiki::NodeRef) -> Result<Option<String>, InsertError> {
        Ok(
            document
                .select("img")
                .map_err(InsertError::HTML)?
                .nth(0)
                .and_then(|a| {
                    a.attributes.borrow().get("src").and_then(
                        |src| Some(src.to_string()),
                    )
                }),
        )
    }
    // Video ads make this a bit messy
    fn get_message(document: &kuchiki::NodeRef) -> Result<Option<String>, InsertError> {
        let selectors = vec![".userContent p", "span"];
        let iters = selectors.iter().map(|s| document.select(s)).flat_map(|a| a);

        Ok(
            iters
                .map(|i| {
                    i.fold(String::new(), |m, a| m + &a.as_node().to_string())
                })
                .filter(|i| i.len() > 0)
                .nth(0),
        )
    }

    fn get_images(document: &kuchiki::NodeRef) -> Result<Option<Vec<String>>, InsertError> {
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
                .collect::<Option<Vec<String>>>(),
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
    fn internal() {
        let ad = include_str!("./html-test.txt");
        let post = AdPost {
            id: "test".to_string(),
            html: ad.to_string(),
            political: true,
            browser_lang: "en-US".to_string(),
        };
        let new_ad = NewAd::new(&post).unwrap();
        assert!(new_ad.thumbnail.is_some());
        assert_eq!(new_ad.images.unwrap().len(), 1);
        assert_eq!(
            new_ad.message.unwrap(),
            "<p><a class=\"_58cn\" href=\"https://www.facebook.com/hashtag/valerian\"><span class=\"_5afx\"><span class=\"_58cl _5afz\">#</span><span class=\"_58cm\">Valerian</span></span></a> is “the best experience since ‘Avatar.’” See it in 3D and RealD3D theaters this Friday. Get tickets now: <a>ValerianTickets.com</a></p>"
        );
        assert!(new_ad.title.is_some());
    }
}
