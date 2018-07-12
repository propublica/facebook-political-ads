extern crate diesel;
extern crate dotenv;
extern crate kuchiki;
extern crate server;

use diesel::pg::PgConnection;
use diesel::prelude::*;
use diesel::dsl::sql;
use diesel::sql_types::Bool;

use dotenv::dotenv;
use kuchiki::traits::*;
use server::models::{get_paid_for_by, Ad};
use server::schema::ads::*;
use server::schema::ads::dsl::*;
use server::start_logging;
use std::env;

fn main() {
    dotenv().ok();
    start_logging();
    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    let conn = PgConnection::establish(&database_url).unwrap();
    let dbads: Vec<Ad> = ads.order(created_at.desc())
        .filter(sql::<Bool>("created_at > '2018-05-24'"))
        .filter(sql::<Bool>("paid_for_by is null"))
        .filter(sql::<Bool>("html ilike '%Paid for by%'"))
        .load::<Ad>(&conn)
        .expect("Couldn't get ads.");
    for ad in dbads {
        let document = kuchiki::parse_html().one(ad.html.clone());
        let parsed_paid_for_by = get_paid_for_by(&document);
        if parsed_paid_for_by.is_some() {
            println!("{:?}", parsed_paid_for_by.clone().unwrap());
            diesel::update(ads.find(ad.id))
                .set(paid_for_by.eq(parsed_paid_for_by.unwrap()))
                .execute(&conn)
                .unwrap();
        }

    }
}
