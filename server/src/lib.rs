#[macro_use]
extern crate diesel;
extern crate dotenv;
#[macro_use]
extern crate diesel_codegen;
extern crate futures;
extern crate futures_cpupool;
#[macro_use]
extern crate lazy_static;
extern crate r2d2;
extern crate r2d2_diesel;

pub mod schema;
pub mod models;
pub mod server;

use diesel::prelude::*;
use diesel::pg::PgConnection;
use futures_cpupool::CpuPool;
use dotenv::dotenv;
use std::env;
use r2d2_diesel::ConnectionManager;
use r2d2::{Pool, Config};

use self::models::{NewAd, Ad};

lazy_static! {
    pub static ref DB_POOL: Pool<ConnectionManager<PgConnection>> = {
        dotenv().ok();
        let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
        let config = Config::default();
        let manager = ConnectionManager::<PgConnection>::new(database_url);
        Pool::new(config, manager).expect("Failed to create pool.")
    };
    pub static ref THREAD_POOL: CpuPool = {
      CpuPool::new_num_cpus()  
    };
}

#[derive(Debug)]
pub enum InsertError {
    Timeout(r2d2::GetTimeout),
    DataBase(diesel::result::Error),
}

pub fn create_ad(
    id: &str,
    html: &str,
    political: bool,
    pool: Pool<ConnectionManager<PgConnection>>,
) -> Result<(), InsertError> {
    use schema::ads;


    let ad = NewAd {
        id: id,
        html: html,
        political: if political { 1 } else { 0 },
        not_political: if !political { 1 } else { 0 },
    };

    let connection = try!(pool.get().map_err(InsertError::Timeout));
    let _: Ad = try!(
        diesel::insert(&ad)
            .into(ads::table)
            .get_result(&*connection)
            .map_err(InsertError::DataBase)
    );
    Ok(())
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {}
}
