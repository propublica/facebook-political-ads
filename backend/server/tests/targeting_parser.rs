extern crate chrono;
extern crate diesel;
extern crate dotenv;
extern crate r2d2;
extern crate rand;
extern crate server;

use diesel::prelude::*;
use server::models::Ad;
use server::schema::ads::dsl::*;
use server::targeting_parser::parse_targeting;

mod common;

#[test]
fn test_parse_targeting() {
    let connection = common::connect();
    common::seed(&connection);
    let adverts = ads.filter(targeting.is_not_null())
        .filter(lang.eq("en-US"))
        .load::<Ad>(&connection)
        .unwrap();
    for ad in adverts {
        let t = ad.clone().targeting.unwrap();
        let ad_targets = parse_targeting(&t);
        if ad_targets.is_err() {
            println!("{:?}", ad.clone().targeting);
            assert!(false);
        }
    }
    common::unseed(&connection);
}

#[test]
fn getting_targets() {
    use server::models::Targets;
    use server::models::Aggregate;
    let connection = common::connect();
    common::seed(&connection);
    common::seed_political(&connection);
    let db_pool = common::connect_pool();
    let t: Vec<Targets> = Targets::get("en-US", &Some(1), None, &db_pool).unwrap();
    assert!(t.get(0).is_some());
    common::unseed(&connection);
}
