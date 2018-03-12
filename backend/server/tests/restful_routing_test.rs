extern crate chrono;
extern crate futures;
extern crate hyper;
extern crate rand;
extern crate serde;
extern crate serde_json;
extern crate server;
extern crate tokio_core;

use futures::{Future, Stream};
use hyper::{Client, Method, Request};
use tokio_core::reactor::Core;
use serde_json::Value;
use hyper::header::{qitem, AcceptLanguage, LanguageTag};
use std::str;

#[test]
fn test_index_url() {
    let mut core = Core::new().unwrap();
    let client = Client::new(&core.handle());
    let uri = "http://localhost:8080/facebook-ads/ads".parse().unwrap();
    let mut req = Request::new(Method::Get, uri);
    let mut langtag: LanguageTag = Default::default();
    langtag.language = Some("en".to_owned());
    langtag.region = Some("US".to_owned());
    req.headers_mut().set(AcceptLanguage(vec![qitem(langtag)]));

    let work = client.request(req).and_then(|res| {
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
    let mut req = Request::new(Method::Get, uri);
    let mut langtag: LanguageTag = Default::default();
    langtag.language = Some("en".to_owned());
    langtag.region = Some("US".to_owned());
    req.headers_mut().set(AcceptLanguage(vec![qitem(langtag)]));

    let work = client.request(req).and_then(|res| {
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

    let mut req = Request::new(Method::Get, uri);
    let mut langtag: LanguageTag = Default::default();
    langtag.language = Some("en".to_owned());
    langtag.region = Some("US".to_owned());
    req.headers_mut().set(AcceptLanguage(vec![qitem(langtag)]));

    let work = client.request(req).and_then(|res| {
        res.body().concat2().and_then(move |body| {
            let v: Value = serde_json::from_slice(&body).unwrap();
            assert_eq!(v["id"], "23842729393560096");
            Ok(())
        })
    });
    core.run(work).unwrap();
}

// ensuring we return the right HTML page for React to do its thing.
#[test]
fn test_admin_url_without_slash() {
    let mut core = Core::new().unwrap();
    let client = Client::new(&core.handle());
    let uri = "http://localhost:8080/facebook-ads/admin".parse().unwrap();
    let mut req = Request::new(Method::Get, uri);
    let mut langtag: LanguageTag = Default::default();
    langtag.language = Some("en".to_owned());
    langtag.region = Some("US".to_owned());
    req.headers_mut().set(AcceptLanguage(vec![qitem(langtag)]));

    let work = client.request(req).and_then(|res| {
        res.body().concat2().and_then(move |body| {
            assert!(
                str::from_utf8(&body)
                    .unwrap()
                    .contains("<title>Admin: Facebook Political Ad Collector</title>")
            );
            Ok(())
        })
    });
    core.run(work).unwrap();
}

#[test]
fn test_admin_url_with_slash() {
    let mut core = Core::new().unwrap();
    let client = Client::new(&core.handle());
    let uri = "http://localhost:8080/facebook-ads/admin/".parse().unwrap();
    let mut req = Request::new(Method::Get, uri);
    let mut langtag: LanguageTag = Default::default();
    langtag.language = Some("en".to_owned());
    langtag.region = Some("US".to_owned());
    req.headers_mut().set(AcceptLanguage(vec![qitem(langtag)]));

    let work = client.request(req).and_then(|res| {
        res.body().concat2().and_then(move |body| {
            assert!(
                str::from_utf8(&body)
                    .unwrap()
                    .contains("<title>Admin: Facebook Political Ad Collector</title>")
            );
            Ok(())
        })
    });
    core.run(work).unwrap();
}

#[test]
fn test_index_html_url_without_slash() {
    let mut core = Core::new().unwrap();
    let client = Client::new(&core.handle());
    let uri = "http://localhost:8080/facebook-ads".parse().unwrap();
    let mut req = Request::new(Method::Get, uri);
    let mut langtag: LanguageTag = Default::default();
    langtag.language = Some("en".to_owned());
    langtag.region = Some("US".to_owned());
    req.headers_mut().set(AcceptLanguage(vec![qitem(langtag)]));

    let work = client.request(req).and_then(|res| {
        res.body().concat2().and_then(move |body| {
            assert!(
                str::from_utf8(&body)
                    .unwrap()
                    .contains("<title>Facebook Political Ad Collector</title>")
            );
            Ok(())
        })
    });
    core.run(work).unwrap();
}

#[test]
fn test_index_html_url_with_slash() {
    let mut core = Core::new().unwrap();
    let client = Client::new(&core.handle());
    let uri = "http://localhost:8080/facebook-ads/".parse().unwrap();
    let mut req = Request::new(Method::Get, uri);
    let mut langtag: LanguageTag = Default::default();
    langtag.language = Some("en".to_owned());
    langtag.region = Some("US".to_owned());
    req.headers_mut().set(AcceptLanguage(vec![qitem(langtag)]));

    let work = client.request(req).and_then(|res| {
        res.body().concat2().and_then(move |body| {
            assert!(
                str::from_utf8(&body)
                    .unwrap()
                    .contains("<title>Facebook Political Ad Collector</title>")
            );
            Ok(())
        })
    });
    core.run(work).unwrap();
}

#[test]
fn test_ad_permalink_url_goes_to_index_html_url() {
    let mut core = Core::new().unwrap();
    let client = Client::new(&core.handle());
    let uri = "http://localhost:8080/facebook-ads/ad/23842672388160612"
        .parse()
        .unwrap();
    let mut req = Request::new(Method::Get, uri);
    let mut langtag: LanguageTag = Default::default();
    langtag.language = Some("en".to_owned());
    langtag.region = Some("US".to_owned());
    req.headers_mut().set(AcceptLanguage(vec![qitem(langtag)]));

    let work = client.request(req).and_then(|res| {
        res.body().concat2().and_then(move |body| {
            assert!(
                str::from_utf8(&body)
                    .unwrap()
                    .contains("<title>Facebook Political Ad Collector</title>")
            );
            Ok(())
        })
    });
    core.run(work).unwrap();
}
