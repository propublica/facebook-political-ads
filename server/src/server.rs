use diesel;
use dotenv::dotenv;
use futures::future;
use futures_cpupool::CpuFuture;
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
use std::string;
use super::{DB_POOL, THREAD_POOL};

use diesel::prelude::*;
use diesel::pg::PgConnection;

use r2d2_diesel::ConnectionManager;
use r2d2::Pool;

pub struct AdServer;

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
        let save = req.body().concat2().map(move |msg| AdServer::save_ad(msg));
        save.map(|_| {
            warn!("wat");
            Response::new()
        }).boxed()
    }

    fn save_ad(msg: Chunk) -> CpuFuture<Ad, InsertError> {
        THREAD_POOL.spawn_fn(move || AdServer::create_ad(msg, DB_POOL.clone()))
    }

    fn create_ad(
        msg: Chunk,
        pool: Pool<ConnectionManager<PgConnection>>,
    ) -> Result<Ad, InsertError> {
        use schema::ads;
        warn!("Inserting ad {:?}", String::from_utf8(msg.to_vec()));
        let string = String::from_utf8(msg.to_vec()).map_err(InsertError::String)?;

        let ad: AdPost = serde_json::from_str(&string).map_err(InsertError::JSON)?;

        let ad = NewAd {
            id: ad.id,
            html: ad.html,
            political: if ad.political { 1 } else { 0 },
            not_political: if !ad.political { 1 } else { 0 },
        };

        let connection = pool.get().map_err(InsertError::Timeout)?;
        let ad: Ad = diesel::insert(&ad)
            .into(ads::table)
            .get_result(&*connection)
            .map_err(InsertError::DataBase)?;

        Ok(ad)
    }

    pub fn start() {
        dotenv().ok();
        pretty_env_logger::init().unwrap();
        let addr = env::var("HOST").expect("HOST must be set").parse().expect(
            "Error parsing HOST",
        );
        let server = Http::new().bind(&addr, || Ok(AdServer)).unwrap();
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
        AdServer::create_ad(Chunk::new)
    }
}
