extern crate diesel;
extern crate dotenv;
extern crate server;
use dotenv::dotenv;
use diesel::pg::PgConnection;
use diesel::prelude::*;
use server::models::{Ad, get_targets};
use server::start_logging;
use server::schema::ads::dsl::*;
use std::env;

fn main() {
    dotenv().ok();
    start_logging();

    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    let conn = PgConnection::establish(&database_url).unwrap();
    let dbads: Vec<Ad> = ads.order(created_at.desc())
        .filter(targeting.is_not_null())
        .load::<Ad>(&conn)
        .unwrap();
    for ad in dbads {
        diesel::update(ads.find(ad.id))
            .set((
                targeting.eq(&ad.targeting),
                targets.eq(get_targets(ad.targeting.clone())),
            ))
            .execute(&conn)
            .unwrap();
    }
}
