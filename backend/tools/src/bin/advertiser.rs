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
use server::targeting_parser::{collect_advertiser, collect_targeting, Targeting};

pub fn get_advertiser(targ: &Option<String>, document: &kuchiki::NodeRef) -> Option<String> {
    match *targ {
        Some(ref targ) => {
            let targeting_advertiser = collect_advertiser(targ);
            println!("{:?}", targeting_advertiser);
            let author_link_advertiser = get_author_link(document).map(|a| a.text_contents()).ok();
            println!("{:?}", author_link_advertiser);
            targeting_advertiser.or(author_link_advertiser)
        },
        None => get_author_link(document).map(|a| a.text_contents()).ok(),
    }
}


fn main() {
    dotenv().ok();
    start_logging();
    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    let conn = PgConnection::establish(&database_url).unwrap();
    let dbads: Vec<Ad> = ads.order(created_at.desc())
        .filter(advertiser.is_null())
        .load::<Ad>(&conn)
        .expect("Couldn't get ads.");
    for ad in dbads {
        println!("looking at {:?}", ad.id);
        let document = kuchiki::parse_html().one(ad.html.clone());
        let advertiser_name = get_advertiser(&ad.targeting, &document);
        if advertiser_name.is_some() {
            println!("found advertiser for {:?} {:?}", ad.id, advertiser_name.clone().unwrap());
            diesel::update(ads.find(ad.id))
                .set(advertiser.eq(advertiser_name.unwrap()))
                .execute(&conn)
                .unwrap();
        }
    }
}
