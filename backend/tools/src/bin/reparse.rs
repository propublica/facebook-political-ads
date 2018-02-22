extern crate diesel;
extern crate dotenv;
extern crate futures_cpupool;
extern crate hyper;
extern crate hyper_tls;
extern crate r2d2;
extern crate server;
extern crate tokio_core;

use diesel::pg::PgConnection;
use diesel::prelude::*;
use dotenv::dotenv;
use futures_cpupool::CpuPool;
use hyper::Client;
use hyper_tls::HttpsConnector;
use diesel::r2d2::ConnectionManager;
use r2d2::Pool;
use server::models::Ad;
use server::start_logging;
use std::env;
use tokio_core::reactor::Core;

fn main() {
    use server::schema::ads::dsl::*;
    dotenv().ok();
    start_logging();

    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    let manager = ConnectionManager::<PgConnection>::new(database_url);
    let db_pool = Pool::builder()
        .build(manager)
        .expect("Failed to create pool.");
    let pool = CpuPool::new_num_cpus();
    let conn = db_pool.get().expect("Failed to get a connection");
    let mut core = Core::new().unwrap();
    let connector = HttpsConnector::new(4, &core.handle()).expect("Couldn't build HttpsConnector");
    let client = Client::configure()
        .connector(connector)
        .build(&core.handle());
    let dbads: Vec<Ad> = ads.order(created_at.desc())
        .load::<Ad>(&*conn)
        .expect("Couldn't get ads");

    for ad in dbads {
        let db_pool = db_pool.clone();
        let pool = pool.clone();
        let client = client.clone();

        let future = ad.grab_and_store(client, &db_pool, &pool);
        let _ = core.run(future);
    }
}
