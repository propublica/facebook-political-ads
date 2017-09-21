#![recursion_limit="256"]
extern crate chrono;
#[macro_use]
extern crate diesel;
extern crate dotenv;
#[macro_use]
extern crate diesel_codegen;
extern crate diesel_full_text_search;
#[macro_use]
extern crate error_chain;
extern crate hyper;
extern crate hyper_tls;
extern crate futures;
extern crate futures_cpupool;
extern crate jsonwebtoken;
extern crate kuchiki;
#[macro_use]
extern crate log;
extern crate log4rs;
extern crate r2d2;
extern crate r2d2_diesel;
extern crate rusoto_core;
extern crate rusoto_credential;
extern crate rusoto_s3;
extern crate reqwest;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
extern crate tokio_core;
extern crate unicase;
extern crate url;

pub mod errors;
pub mod models;
pub mod schema;
pub mod server;

use log::LogLevelFilter;
use log4rs::append::console::ConsoleAppender;
use log4rs::config::{Appender, Config as LogConfig, Logger, Root};
use std::string;

pub fn start_logging() {
    let stdout = ConsoleAppender::builder().build();
    let config = LogConfig::builder()
        .appender(Appender::builder().build("stdout", Box::new(stdout)))
        .logger(Logger::builder().build("hyper", LogLevelFilter::Info))
        .logger(Logger::builder().build("server", LogLevelFilter::Info))
        .build(Root::builder().appender("stdout").build(
            LogLevelFilter::Error,
        ))
        .expect("Log config didn't work");
    log4rs::init_config(config).expect("Logging encountered an error.");
}
