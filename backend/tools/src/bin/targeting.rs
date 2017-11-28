extern crate diesel;
extern crate dotenv;
extern crate server;
use dotenv::dotenv;
use diesel::pg::PgConnection;
use diesel::prelude::*;
use server::models::Ad;
use server::start_logging;
use server::targeting_parser::{collect_targeting, collect_advertiser, NewTargeting};
use server::schema::ads::dsl::*;
use server::schema::targeting_info::dsl::*;
use server::schema::ads_targeting::dsl::*;
use std::env;

fn main() {
    dotenv().ok();
    start_logging();

    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    let conn = PgConnection::establish(&database_url).unwrap();
    let dbads: Vec<Ad> = ads.order(created_at.desc())
        .filter(targeting.is_not_null())
        .limit(1)
        .load::<Ad>(&conn)
        .unwrap();
    for ad in dbads {
        let info = &ad.targeting.unwrap();
        let targets = collect_targeting(&info);
        println!("{:?}", targets);
        for targt in targets.unwrap() {}

        // println!("{:?}", collect_advertiser(&targets));
    }
}
