extern crate chrono;
extern crate server;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;

use chrono::DateTime;
use chrono::offset::Utc;
use server::models::Ad;

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
    #[serde(skip_serializing)]
    pub suppressed: bool,
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
            ..ad
        }
    }
}

fn main() {
    
}
