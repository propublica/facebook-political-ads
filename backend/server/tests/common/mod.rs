extern crate server;

use dotenv::dotenv;
use std::env;
use std::str::FromStr;
use diesel;
use diesel::prelude::*;
use diesel::pg::PgConnection;
use chrono::offset::Utc;
use rand::{thread_rng, Rng};
use diesel::r2d2::ConnectionManager;
use r2d2::Pool;
use server::models::*;
use server::schema::ads;
use server::schema::ads::dsl::*;
use server::server::AdPost;

pub fn connect() -> PgConnection {
    dotenv().ok();
    let database_url = env::var("TEST_DATABASE_URL").unwrap();
    PgConnection::establish(&database_url).unwrap()
}

pub fn connect_pool() -> Pool<ConnectionManager<PgConnection>> {
    dotenv().ok();
    let database_url = env::var("TEST_DATABASE_URL").unwrap();
    let manager = ConnectionManager::<PgConnection>::new(database_url.clone());
    Pool::builder()
        .build(manager)
        .expect("Failed to create pool.")
}

pub fn seed(connection: &PgConnection) {
    unseed(connection);

    let mock_html = include_str!("html-test.txt");
    let en = String::from("en-US");

    let post: AdPost = AdPost {
        id: "1".to_string(),
        html: mock_html.to_string(),
        political: None,
        targeting: Some(make_targeting()),
    };

    let new_ad: NewAd = NewAd::new(&post, &en).unwrap();
    let _ = diesel::insert_into(ads::table)
        .values(&new_ad)
        .on_conflict(id)
        .do_update()
        .set(updated_at.eq(Utc::now()))
        .execute(connection)
        .unwrap();
}

pub fn seed_political(connection: &PgConnection) {
    seed(connection);

    let _ = diesel::update(ads.find("1".to_string()))
        .set(political_probability.eq(1.0))
        .execute(connection)
        .unwrap();
}

pub fn unseed(connection: &PgConnection) {
    diesel::delete(ads)
        .execute(connection)
        .expect("Error unseeding");
}

fn make_targeting() -> String {
    let mut rng = thread_rng();

    let targetings_str = include_str!("targetings.txt")
        .lines()
        .map(|s| String::from_str(s).expect("Error reading string"))
        .collect::<Vec<_>>();

    rng.choose(&targetings_str).unwrap().clone()
}
