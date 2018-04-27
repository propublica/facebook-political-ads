extern crate diesel;
extern crate dotenv;
extern crate kuchiki;
extern crate server;
use dotenv::dotenv;
use diesel::pg::PgConnection;
use diesel::dsl::sql;
use diesel::sql_types::Bool;
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
        .filter(political_probability.gt(0.70))
        .filter(lang.eq("en-US"))
        .filter(suppressed.eq(false))
        // .filter(sql::<Bool>("targeting ilike '%part of an audience%'"))
        .load::<Ad>(&conn)
        .unwrap();
    for ad in dbads {
        let document = kuchiki::parse_html().one(ad.html.clone());
        // println!("{:?}", ad.targeting);
        // println!("{:?}", get_targets(&ad.targeting));
        // println!("");

        // find any cases where the revised targeting parser removes an element
        if let Some(ref old_targets) = ad.targets {
            if let Some(new_targets) = get_targets(&ad.targeting) {
                if old_targets.to_string().len() > new_targets.to_string().len() {
                    println!("lost target -->");
                    println!("{:?}", ad.targeting);
                    println!("{:?}", get_targets(&ad.targeting));
                    println!("{:?}", old_targets);
                    println!("");
                }
            }
        }

        diesel::update(ads.find(ad.id))
            .set((
                targets.eq(get_targets(&ad.targeting)),
                advertiser.eq(get_advertiser(&ad.targeting, &document)),
            ))
            .execute(&conn)
            .unwrap();
    }
}
