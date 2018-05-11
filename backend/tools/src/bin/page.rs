extern crate diesel;
extern crate dotenv;
extern crate kuchiki;
extern crate server;

use diesel::pg::PgConnection;
use diesel::prelude::*;
use dotenv::dotenv;
use kuchiki::traits::*;
use server::models::{get_author_link, Ad};
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
        .filter(page.is_null())
        .load::<Ad>(&conn)
        .expect("Couldn't get ads.");
    println!("{:?}", "got ads");
    for ad in dbads {
        let document = kuchiki::parse_html().one(ad.html.clone());
        println!("{:?}", ad.id);
        let html_page = get_author_link(&document)
            .ok()
            .and_then(|l| l.attributes.borrow().get("href").map(|i| i.to_string()));
        if html_page.is_some() {
            diesel::update(ads.find(ad.id))
                .set(page.eq(html_page.unwrap()))
                .execute(&conn)
                .unwrap();
        }
    }
}
