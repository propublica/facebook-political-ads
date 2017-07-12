#[macro_use]
extern crate diesel;
extern crate dotenv;
#[macro_use]
extern crate diesel_codegen;
extern crate hyper;
extern crate futures;
extern crate futures_cpupool;
#[macro_use]
extern crate log;
extern crate pretty_env_logger;
extern crate r2d2;
extern crate r2d2_diesel;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;

pub mod schema;
pub mod models;
pub mod server;

use dotenv::dotenv;
use std::env;

use r2d2_diesel::ConnectionManager;
use r2d2::{Pool, Config};
use diesel::pg::PgConnection;
use futures_cpupool::CpuPool;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {}
}
