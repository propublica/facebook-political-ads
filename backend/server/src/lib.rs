#![recursion_limit = "256"]
extern crate chrono;
#[macro_use]
extern crate diesel;
extern crate diesel_full_text_search;
extern crate dotenv;
#[macro_use]
extern crate error_chain;
extern crate futures;
extern crate futures_cpupool;
extern crate hyper;
extern crate hyper_tls;
extern crate kuchiki;
extern crate log4rs;
#[macro_use]
extern crate log;
#[macro_use]
extern crate nom;
extern crate r2d2;
extern crate r2d2_diesel;
extern crate regex;
extern crate rusoto_core;
extern crate rusoto_credential;
extern crate rusoto_s3;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate tokio_core;
extern crate tokio_postgres;
extern crate tokio_timer;
extern crate unicase;
extern crate url;

pub mod errors;
pub mod models;
pub mod schema;
pub mod server;
pub mod targeting_parser;

use log::LevelFilter;
use log4rs::append::console::ConsoleAppender;
use log4rs::config::{Appender, Config as LogConfig, Logger, Root};
use std::string;

pub fn start_logging() {
    let stdout = ConsoleAppender::builder().build();
    let config = LogConfig::builder()
        .appender(Appender::builder().build("stdout", Box::new(stdout)))
        .logger(Logger::builder().build("hyper", LevelFilter::Info))
        .logger(Logger::builder().build("server", LevelFilter::Info))
        .build(Root::builder().appender("stdout").build(LevelFilter::Error))
        .expect("Log config didn't work");
    log4rs::init_config(config).expect("Logging encountered an error.");
}
