extern crate chrono;
extern crate futures;
extern crate hyper;
extern crate rand;
extern crate serde;
extern crate serde_json;
extern crate server;
extern crate tokio_core;

use futures::{Future, Stream};
use hyper::Client;
use tokio_core::reactor::Core;
use serde_json::Value;

#[test]
fn test_index_url() {
    let mut core = Core::new().unwrap();
    let client = Client::new(&core.handle());
    let uri = "http://localhost:8080/facebook-ads/ads".parse().unwrap();
    let work = client.get(uri).and_then(|res| {
        res.body().concat2().and_then(move |body| {
            let v: Value = serde_json::from_slice(&body).unwrap();
            assert!(v["ads"].as_array().unwrap().len() >= 1);
            Ok(())
        })
    });
    core.run(work).unwrap();
}

#[test]
fn test_index_url_with_slash() {
    let mut core = Core::new().unwrap();
    let client = Client::new(&core.handle());
    let uri = "http://localhost:8080/facebook-ads/ads/".parse().unwrap();
    let work = client.get(uri).and_then(|res| {
        res.body().concat2().and_then(move |body| {
            let v: Value = serde_json::from_slice(&body).unwrap();
            assert!(v["ads"].as_array().unwrap().len() >= 1);
            Ok(())
        })
    });
    core.run(work).unwrap();
}

#[test]
fn test_individual_ad_url() {
    let mut core = Core::new().unwrap();
    let client = Client::new(&core.handle());
    let uri = "http://localhost:8080/facebook-ads/ads/23842729393560096"
        .parse()
        .unwrap();
    let work = client.get(uri).and_then(|res| {
        res.body().concat2().and_then(move |body| {
            let v: Value = serde_json::from_slice(&body).unwrap();
            assert_eq!(v["ads"].as_array().unwrap().len(), 1);
            Ok(())
        })
    });
    core.run(work).unwrap();
}
