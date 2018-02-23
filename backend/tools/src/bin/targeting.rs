extern crate diesel;
extern crate dotenv;
extern crate kuchiki;
extern crate server;
use dotenv::dotenv;
use diesel::pg::PgConnection;
use diesel::prelude::*;
use kuchiki::traits::*;
use server::models::{get_advertiser, get_targets, Ad};
use server::start_logging;
use server::schema::ads::dsl::*;
use std::env;

fn main() {
    dotenv().ok();
    start_logging();

    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    let conn = PgConnection::establish(&database_url).unwrap();
    let dbads: Vec<Ad> = ads.order(created_at.desc())
        //.filter(political_probability.gt(0.70))
        .filter(suppressed.eq(false))
        .load::<Ad>(&conn)
        .unwrap();
    for ad in dbads {
        let document = kuchiki::parse_html().one(ad.html.clone());
        diesel::update(ads.find(ad.id))
            .set((
                targets.eq(get_targets(&ad.targeting)),
                advertiser.eq(get_advertiser(&ad.targeting, &document)),
            ))
            .execute(&conn)
            .unwrap();
    }
}
