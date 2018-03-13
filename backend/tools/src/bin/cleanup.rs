extern crate diesel;
extern crate dotenv;
extern crate kuchiki;
extern crate rusoto_core;
extern crate rusoto_credential;
extern crate rusoto_s3;
extern crate server;
extern crate url;

use diesel::pg::PgConnection;
use diesel::prelude::*;
use dotenv::dotenv;
use kuchiki::traits::*;
use rusoto_core::Region;
use rusoto_s3::{DeleteObjectRequest, S3, S3Client};
use server::models::*;
use server::start_logging;
use std::env;
use url::Url;

fn cleanup(ad: &mut Ad) -> bool {
    let document = kuchiki::parse_html().one(ad.html.clone());
    let mut ret = false;
    for like in document.select("h5._1qbu").unwrap() {
        like.as_node().detach();
        ret = true;
    }

    let mut images = ad.images.clone();
    for img in document.select(".commentable_item img").unwrap() {
        if let Some(src) = img.attributes.borrow_mut().get_mut("src") {
            if let Some(pos) = images.iter().position(|x| x == src) {
                images.remove(pos);
            }

            println!("deleting {}", src);
            if let Ok(url) = src.parse::<Url>() {
                let client = S3Client::simple(Region::UsEast1);
                let res = client
                    .delete_object(&DeleteObjectRequest {
                        bucket: "pp-facebook-ads".to_string(),
                        key: url.path().trim_left_matches('/').to_string(),
                        ..DeleteObjectRequest::default()
                    })
                    .sync();
                *src = "".to_string();
                if res.is_err() {
                    println!("Couldn't delete {} {:?}", src, res);
                }
            }
        }
        ret = true;
    }

    for comment in document.select(".commentable_item").unwrap() {
        comment.as_node().detach();
        ret = true;
    }

    ad.html = document
        .select("div")
        .unwrap()
        .nth(0)
        .unwrap()
        .as_node()
        .to_string();
    ad.images = images;
    ret
}

fn main() {
    use server::schema::ads::dsl::*;
    dotenv().ok();
    start_logging();

    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    let conn = PgConnection::establish(&database_url).unwrap();
    let dbads: Vec<Ad> = ads.order(created_at.desc()).load::<Ad>(&conn).unwrap();
    for mut ad in dbads {
        if cleanup(&mut ad) {
            let document = kuchiki::parse_html().one(ad.html.clone());
            println!("Cleaned {}", ad.id);
            diesel::update(ads.find(ad.id.clone()))
                .set((
                    html.eq(ad.html),
                    title.eq(get_title(&document).unwrap()),
                    message.eq(get_message(&document).unwrap()),
                    images.eq(ad.images),
                ))
                .execute(&conn)
                .unwrap();
        } else {
            println!("Skipped {}", ad.id);
        }
    }
}
