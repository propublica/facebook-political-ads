#![recursion_limit="256"]
extern crate chrono;
#[macro_use]
extern crate diesel;
extern crate dotenv;
#[macro_use]
extern crate diesel_codegen;
extern crate hyper;
extern crate hyper_tls;
extern crate futures;
extern crate futures_cpupool;
extern crate kuchiki;
#[macro_use]
extern crate log;
extern crate log4rs;
extern crate r2d2;
extern crate r2d2_diesel;
extern crate rusoto_core;
extern crate rusoto_credential;
extern crate rusoto_s3;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
extern crate tokio_core;

pub mod models;
pub mod schema;
pub mod server;

use std::string;

// This is basically a catch all for all the Errors we think we'll ever see to
// combat type restrictions on all of the hyper/futures function calls.
#[derive(Debug)]
pub enum InsertError {
    Timeout(r2d2::GetTimeout),
    DataBase(diesel::result::Error),
    JSON(serde_json::Error),
    String(string::FromUtf8Error),
    Hyper(hyper::Error),
    Uri(hyper::error::UriError),
    HTML(()),
    TLS(rusoto_core::TlsError),
    S3(rusoto_s3::PutObjectError),
    AWS(rusoto_credential::CredentialsError),
    Language(()),
}
