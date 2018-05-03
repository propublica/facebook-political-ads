extern crate diesel;
extern crate dotenv;
extern crate kuchiki;
extern crate server;
use dotenv::dotenv;
use diesel::pg::PgConnection;
use diesel::dsl::{not, sql};
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
        .filter(not(suppressed.eq(true)))
        .filter(sql::<Bool>("targeting is not null"))
        // .filter(sql::<Bool>("targeting ilike '%part of an audience%'"))
        .load::<Ad>(&conn)
        .unwrap();
    let mut total = 0;
    let mut still_parses_correctly = 0;
    let mut newly_parses_correctly = 0;
    let mut parse_failed = 0;
    let mut parse_got_worse = 0;
    let mut still_doesnt_parse = 0;
    for ad in dbads {
        total += 1;
        let document = kuchiki::parse_html().one(ad.html.clone());
        // find any cases where the revised targeting parser removes an element
        ;
        if let Some(ref old_targets) = ad.targets {
            if let Some(new_targets) = get_targets(&ad.targeting) {
                if old_targets.to_string().len() > new_targets.to_string().len() {
                    println!("lost target -->");
                    println!("{:?}", ad.targeting);
                    println!("{:?}", get_targets(&ad.targeting));
                    println!("{:?}", old_targets);
                    println!("");
                    parse_got_worse += 1;
                } else {
                    still_parses_correctly += 1;
                }
            } else {
                if old_targets.as_array().unwrap().len() == 0 {
                    still_doesnt_parse += 1;
                } else {
                    parse_failed += 1;
                }
            }
        } else {
            if let Some(new_targets) = get_targets(&ad.targeting) {
                if new_targets.as_array().unwrap().len() > 0 {
                    newly_parses_correctly += 1;
                    println!("{:?}", new_targets.as_array().unwrap())
                // how to parse in Ruby.
                // output = open("targeting_output.txt", 'r').read.gsub("\n24865/24877 parse successfully\n1520/24877 now parse correctly\n0/24877 now parse worse than before\n0/24877 newly fail\n12/24877 still fail\n", "")
                // lines = output.gsub("Object(", "").gsub("String(", "").gsub('")', '"').gsub(')}', '').gsub("})", "}").gsub('\\\\"', '"').split("\n")
                // parsed_lines = lines.map{|line| JSON.parse(line)}
                } else {
                    still_doesnt_parse += 1;
                }
            } else {
                still_doesnt_parse += 1;
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
    println!(
        "{:?}/{:?} parse successfully",
        still_parses_correctly + newly_parses_correctly + parse_got_worse,
        total
    );
    println!(
        "{:?}/{:?} now parse correctly",
        newly_parses_correctly, total
    );
    println!(
        "{:?}/{:?} now parse worse than before",
        parse_got_worse, total
    );
    println!("{:?}/{:?} newly fail", parse_failed, total);
    println!("{:?}/{:?} still fail", still_doesnt_parse, total);
}
