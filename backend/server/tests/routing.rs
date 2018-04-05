extern crate chrono;
extern crate diesel;
extern crate dotenv;
extern crate futures;
extern crate hyper;
#[macro_use]
extern crate lazy_static;
extern crate r2d2;
extern crate rand;
extern crate reqwest;
extern crate serde_json;
extern crate server;
extern crate tokio_core;
mod common;

use dotenv::dotenv;
use futures::{oneshot, Future, Stream};
use hyper::server::Http;
use serde_json::Value;
use reqwest::header::{qitem, AcceptLanguage, LanguageTag};
use reqwest::{Client, Response};
use server::server::AdServer;
use std::{env, thread};
use std::default::Default;
use std::sync::Mutex;
use tokio_core::reactor::Core;
use std::sync::mpsc::channel;

lazy_static! {
    static ref LOCK: Mutex<()> = Mutex::new(());
}

fn http_test<T>(url: &str, test: T)
where
    T: Fn(Response),
{
    let _locked = LOCK.lock().unwrap();
    let conn = common::connect();
    dotenv().ok();
    common::seed(&conn);
    common::seed_political(&conn);
    let (tx, rx) = oneshot();
    let (start_tx, start_rx) = channel();
    thread::spawn(move || {
        let mut core = Core::new().unwrap();
        let handle = core.handle();
        let addr = "127.0.0.1:0".parse().unwrap();
        let url = env::var("TEST_DATABASE_URL").unwrap();
        let server_handle = handle.clone();
        let server = Http::new()
            .serve_addr_handle(&addr, &server_handle, || {
                Ok(AdServer::new(handle.clone(), url.clone()))
            })
            .unwrap();
        let h2 = handle.clone();
        let server_addr = server.incoming_ref().local_addr();
        let srv = server
            .for_each(move |conn| {
                h2.spawn(conn.map(|_| ()).map_err(|_| ()));
                Ok(())
            })
            .map_err(|_| ());
        start_tx.send(server_addr).unwrap();
        let _ = core.run(rx.then(|_| Ok(())).select(srv));
    });
    let local = start_rx.recv().unwrap();
    let body = Client::new()
        .get(&format!("http://{}{}", local, url))
        .header(AcceptLanguage(vec![
            qitem(LanguageTag {
                language: Some("en".to_owned()),
                region: Some("US".to_owned()),
                ..Default::default()
            }),
        ]))
        .send()
        .unwrap();
    test(body);
    tx.send(true).unwrap();
    common::unseed(&conn);
}

fn ads(mut response: Response) {
    let json: Result<Value, reqwest::Error> = response.json();
    assert!(json.unwrap()["ads"].as_array().unwrap().len() > 0);
}

#[test]
fn test_ads_url() {
    http_test("/facebook-ads/ads", ads);
    http_test("/facebook-ads/ads/", ads);
}

#[test]
fn test_individual_ad_url() {
    http_test("/facebook-ads/ads/1", |mut resp| {
        let json: Result<Value, reqwest::Error> = resp.json();
        assert_eq!(json.unwrap()["id"].as_str().unwrap(), "1");
    });
}

fn admin(mut response: Response) {
    let text = response.text().unwrap();
    assert!(text.contains("<title>Admin: Facebook Political Ad Collector</title>"));
}

#[test]
fn test_admin_url() {
    http_test("/facebook-ads/admin", admin);
    http_test("/facebook-ads/admin/", admin);
}

fn index(mut response: Response) {
    let text = response.text().unwrap();
    assert!(text.contains("<title>Facebook Political Ad Collector</title>"));
}

#[test]
fn test_index_url() {
    http_test("/facebook-ads", index);
    http_test("/facebook-ads/", index);
    http_test("/facebook-ads/ad/1", index);
}
