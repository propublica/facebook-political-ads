extern crate chrono;
extern crate csv;
extern crate diesel;
extern crate dotenv;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate server;

use chrono::DateTime;
use chrono::offset::Utc;
use diesel::prelude::*;
use diesel::pg::PgConnection;
use dotenv::dotenv;
use server::start_logging;
use server::models::Ad;
use std::env;
use std::io;

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
    pub images: Option<String>,
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
            advertiser: ad.advertiser,
            page: ad.page,
        }
    }
}

fn main() {
    dotenv().ok();
    start_logging();
    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    let conn = PgConnection::establish(&database_url).unwrap();
    let mut wtr = csv::Writer::from_writer(io::stdout());
    let query = Ad::scoped("en-US");
    let dbads = query.load::<Ad>(&conn).unwrap();
    for ad in dbads {
        wtr.serialize(Record::from(ad)).unwrap();
    }
    wtr.flush().unwrap();
}
