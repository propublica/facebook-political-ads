extern crate diesel;
extern crate dotenv;
extern crate kuchiki;
extern crate server;

use diesel::pg::PgConnection;
use diesel::prelude::*;
use dotenv::dotenv;
use kuchiki::traits::*;
use server::models::*;
use server::start_logging;
use std::env;

fn cleanup(html: String) -> String {
    let document = kuchiki::parse_html().one(html);
    let likes = document.select("h5._1qbu").unwrap();
    for like in likes {
        like.as_node().detach();
    }
    document
        .select("div")
        .unwrap()
        .nth(0)
        .unwrap()
        .as_node()
        .to_string()
}

fn main() {
    use server::schema::ads::dsl::*;
    dotenv().ok();
    start_logging();

    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    let conn = PgConnection::establish(&database_url).unwrap();
    let dbads: Vec<Ad> = ads.order(created_at.desc()).load::<Ad>(&conn).unwrap();
    for ad in dbads {
        let htm = cleanup(ad.html);
        let document = kuchiki::parse_html().one(htm.clone());
        println!("Cleaned {}", ad.id);
        diesel::update(ads.find(ad.id.clone()))
            .set((
                html.eq(htm),
                title.eq(get_title(&document).unwrap()),
                message.eq(get_message(&document).unwrap()),
            ))
            .execute(&conn)
            .unwrap();
    }
}
