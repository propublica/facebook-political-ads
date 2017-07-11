extern crate hyper;
extern crate futures;

use futures::future;
use futures::future::{FutureResult, Map, Either};
use futures::stream::Concat;
use hyper::{Method, StatusCode};
use hyper::server::{Request, Response, Service};

struct AdServer;

impl Service for AdServer {
    type Request = Request;
    type Response = Response;
    type Error = hyper::Error;
    type Future = Either<
        FutureResult<Self::Response, Self::Error>,
        Map<Concat<Body>, fn(Chunk) -> Self::Response>,
    >;

    fn call(&self, _req: Request) -> Self::Future {
        match (req.method(), req.path()) {
            (&Method::Post, "/ads") => Either::B(req.body().concat().map()),

            _ => {
                Either::A(futures::future::ok(
                    Response::new().with_status(StatusCode::NotFound),
                ))
            }
        }
    }
}
