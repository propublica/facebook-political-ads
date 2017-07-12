use diesel;
use dotenv::dotenv;
use futures::future;
use futures_cpupool::{CpuFuture, CpuPool};
use futures::future::{FutureResult, BoxFuture, Either};
use futures::{Future, Stream};
use hyper;
use hyper::{Method, StatusCode, Chunk};
use hyper::server::{Http, Request, Response, Service};
use models::{Ad, NewAd};
use pretty_env_logger;
use serde_json;
use r2d2;
use std::env;
use std::io;
use std::string;

use diesel::prelude::*;
use diesel::pg::PgConnection;

use r2d2_diesel::ConnectionManager;
use r2d2::{Pool, Config};

pub struct AdServer {
    db_pool: Pool<ConnectionManager<PgConnection>>,
    pool: CpuPool,
}

#[derive(Deserialize)]
struct AdPost<'a> {
    id: &'a str,
    html: &'a str,
    political: bool,
}

#[derive(Debug)]
pub enum InsertError {
    Timeout(r2d2::GetTimeout),
    DataBase(diesel::result::Error),
    JSON(serde_json::Error),
    String(string::FromUtf8Error),
    Hyper(hyper::Error),
}

impl Service for AdServer {
    type Request = Request;
    type Response = Response;
    type Error = hyper::Error;
    type Future = Either<
        FutureResult<Self::Response, Self::Error>,
        BoxFuture<Self::Response, Self::Error>,
    >;

    fn call(&self, req: Request) -> Self::Future {
        match (req.method(), req.path()) {
            (&Method::Post, "/ads") => Either::B(self.process_ad(req)),
            _ => {
                Either::A(future::ok(
                    Response::new().with_status(StatusCode::NotFound),
                ))
            }
        }
    }
}


impl AdServer {
    fn process_ad(&self, req: Request) -> BoxFuture<Response, hyper::Error> {
        let db_pool = self.db_pool.clone();
        let pool = self.pool.clone();
        req.body()
            .concat2()
            .then(move |msg| {
                pool.spawn_fn(move || match AdServer::save_ad(msg, db_pool) {
                    Ok(r) => Ok(r),
                    Err(e) => {
                        warn!("{:?}", e);
                        Ok(Response::new().with_status(StatusCode::BadRequest))
                    }
                })
            })
            .boxed()
    }

    fn save_ad(
        msg: Result<Chunk, hyper::Error>,
        db_pool: Pool<ConnectionManager<PgConnection>>,
    ) -> Result<Response, InsertError> {
        use schema::ads;
        let bytes = msg.map_err(InsertError::Hyper)?;
        let string = String::from_utf8(bytes.to_vec()).map_err(
            InsertError::String,
        )?;

        let ad: AdPost = serde_json::from_str(&string).map_err(InsertError::JSON)?;

        let ad = NewAd {
            id: ad.id,
            html: ad.html,
            political: if ad.political { 1 } else { 0 },
            not_political: if !ad.political { 1 } else { 0 },
        };

        let connection = db_pool.get().map_err(InsertError::Timeout)?;
        let _: Ad = diesel::insert(&ad)
            .into(ads::table)
            .get_result(&*connection)
            .map_err(InsertError::DataBase)?;

        Ok(Response::new())
    }

    pub fn start() {
        dotenv().ok();
        pretty_env_logger::init().unwrap();
        let addr = env::var("HOST").expect("HOST must be set").parse().expect(
            "Error parsing HOST",
        );

        let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
        let config = Config::default();
        let manager = ConnectionManager::<PgConnection>::new(database_url);
        let db_pool = Pool::new(config, manager).expect("Failed to create pool.");

        let pool = CpuPool::new_num_cpus();

        let server = Http::new()
            .bind(&addr, move || {
                Ok(AdServer {
                    pool: pool.clone(),
                    db_pool: db_pool.clone(),
                })
            })
            .unwrap();

        println!(
            "Listening on http://{} with 1 thread.",
            server.local_addr().unwrap()
        );
        server.run().unwrap();
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_inserts_an_ad() {
        //   AdServer::create_ad(Chunk::new)
    }
}
