extern crate csv;
extern crate diesel;
#[macro_use]
extern crate serde_derive;

#[derive(Debug, Serialize)]
struct Record {
    pub id: String,
    pub html: String,
    pub political: i32,
    pub not_political: i32,
    pub title: String,
    pub message: String,
    pub thumbnail: String,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub lang: String,
    pub images: Vec<String>,
    pub impressions: i32,
    pub political_probability: f64,
    pub targeting: Option<String>,
    pub targets: Option<String>,
    pub advertiser: Option<String>,
    pub entities: Option<String>,
    pub page: Option<String>,
}

impl From<Ad> for Record {
    fn from(ad: Ad) -> Self {
        Record {
            targets: serde_json::to_string(&ad.targets).ok(),
            entities: serde_json::to_string(&ad.entities).ok(),
            images: serde_json::to_string(&ad.images).ok(),
            id: ad.id,
            html: ad.html,
            political: ad.political,
            not_political: ad.not_political,
            title: ad.title,
            message: ad.message,
            thumbnail: ad.thumbnail,
            created_at: ad.created_at,
            updated_at: ad.updated_at,
            lang: ad.lang,
            impressions: ad.impressions,
            political_probability: ad.political_probability,
            targeting: ad.targeting,
            page: ad.page
        }
    }
}


fn main() {
    dotenv().ok();
    start_logging();
    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    let conn = PgConnection::establish(&database_url).unwrap();
    
}
