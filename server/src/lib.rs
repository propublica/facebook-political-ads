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
extern crate pretty_env_logger;
extern crate r2d2;
extern crate r2d2_diesel;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;

pub mod image_getter;
pub mod models;
pub mod schema;
pub mod server;

use std::string;

#[derive(Debug)]
pub enum InsertError {
    Timeout(r2d2::GetTimeout),
    DataBase(diesel::result::Error),
    JSON(serde_json::Error),
    String(string::FromUtf8Error),
    Hyper(hyper::Error),
    HTML(()),
}
