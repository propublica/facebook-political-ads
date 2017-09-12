extern crate classifier;
extern crate diesel;
extern crate docopt;
extern crate dotenv;
extern crate kuchiki;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate server;

use classifier::Classifier;
use diesel::prelude::*;
use docopt::Docopt;
use dotenv::dotenv;
use kuchiki::traits::*;
use server::models::Ad;
use server::schema::ads::dsl::*;
use std::env;

const USAGE: &'static str = "
Classify the ads we've seen already.

Usage:
  classify <language> <path>
";


#[derive(Debug, Deserialize)]
struct Args {
    arg_path: String,
    arg_language: String,
}

// For now we grab all the ads in the database and classify and resave them, not
// ideal in the future, but we're launching next week.
fn main() {
    dotenv().ok();
    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    let conn = PgConnection::establish(&database_url).expect("could not connect to the database");
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.deserialize())
        .unwrap_or_else(|e| e.exit());
    let classifier =
        Classifier::from_json(&args.arg_path).expect(&format!("Could not open {}", args.arg_path));

    let dbads: Vec<Ad> = ads.filter(lang.eq(args.arg_language))
        .load::<Ad>(&conn)
        .expect("Couldn't get ads");

    for ad in dbads {
        let parsed = kuchiki::parse_html().one(ad.html.clone());
        let mut s = String::new();
        // from: https://docs.rs/kuchiki/0.5.1/src/kuchiki/tree.rs.html#266
        // We do this so that we can insert spaces between text nodes
        for text_node in parsed.inclusive_descendants().text_nodes() {
            s.push_str(" ");
            s.push_str(&text_node.borrow());
        }
        let score = classifier.predict_likelihoods(&s)[1];
        println!("saving {} with score {}", ad.id, score);
        let aid = ad.id.clone();
        diesel::update(ads.find(ad.id))
            .set(political_probability.eq(score))
            .returning(id)
            .get_result::<String>(&conn)
            .expect(&format!("Couldn't save ad {}", aid));
    }
}
