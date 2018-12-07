use diesel::pg::PgConnection;
use diesel::r2d2::ConnectionManager;
use dotenv::dotenv;
use errors::*;
use futures::future;
use futures_cpupool::CpuPool;
use futures::future::{Either, FutureResult};
use futures::{Future, Sink, Stream};
use hyper;
use hyper::{Body, Chunk, Client, Method, StatusCode};
use hyper::client::HttpConnector;
use hyper::server::{Http, Request, Response, Service};
use hyper::header::{AcceptLanguage, AccessControlAllowOrigin, Authorization, Bearer, CacheControl,
                    CacheDirective, Connection as HttpConnection, Location, ContentLength, ContentType, Vary};
use hyper::mime;
use hyper_tls::HttpsConnector;
use jsonwebtoken::{decode, Validation};
use models::{Ad, Advertisers, Aggregate, Entities, NewAd, Segments, Targets};
use r2d2::Pool;
use regex::Regex;
use regex::RegexSet;
use start_logging;
use serde_json;
use serde::ser::Serialize;
use std::collections::HashMap;
use std::env;
use std::fmt::Debug;
use std::error::Error as StdError;
use std::io::ErrorKind as StdIoErrorKind;
use std::io::Error as StdIoError;
use std::fs::File;
use std::io::Read;
use std::time::Duration;
use tokio_core::reactor::{Core, Handle};
use tokio_postgres::{Connection, TlsMode};
use tokio_timer::Timer;
use unicase::Ascii;
use url::{form_urlencoded, Url};

#[derive(Clone)]
pub struct AdServer {
    db_pool: Pool<ConnectionManager<PgConnection>>,
    pool: CpuPool,
    handle: Handle,
    client: Client<HttpsConnector<HttpConnector>>,
    database_url: String,
}

#[derive(Deserialize, Debug)]
pub struct AdPost {
    pub id: String,
    pub html: String,
    pub political: Option<bool>,
    pub targeting: Option<String>,
}

#[derive(Deserialize, Debug)]
pub struct Admin {
    pub username: String,
}

#[derive(Serialize, Debug)]
pub struct ApiResponse {
    ads: Vec<Ad>,
    targets: Vec<Targets>,
    entities: Vec<Entities>,
    advertisers: Vec<Advertisers>,
    total: i64,
}

impl Service for AdServer {
    type Request = Request;
    type Response = Response;
    type Error = hyper::Error;
    type Future = Either<
        FutureResult<Self::Response, Self::Error>,
        Box<Future<Item = Self::Response, Error = Self::Error>>,
    >;
    // This is not at all RESTful, but I'd rather not deal with regexes. Maybe soon
    // someone will make a hyper router that's nice.
    fn call(&self, req: Request) -> Self::Future {
        match (req.method(), req.path()) {
            // Admin
            (&Method::Get, "/facebook-ads/admin.js") => {
                Either::B(self.file("public/dist/admin.js", ContentType(mime::TEXT_JAVASCRIPT)))
            }
            (&Method::Get, "/facebook-ads/admin.js.map") => {
                Either::B(self.file("public/dist/admin.js.map", ContentType::json()))
            }
            (&Method::Get, "/facebook-ads/admin/styles.css") => {
                Either::B(self.file("public/dist/admin.css", ContentType(mime::TEXT_CSS)))
            }
            (&Method::Get, "/facebook-ads/images/share-fb.png") => {
                Either::B(self.file_bytes("public/images/share-fb.png", ContentType(mime::IMAGE_PNG)))
            }
            (&Method::Get, "/facebook-ads/images/share-twitter.png") => {
                Either::B(self.file_bytes("public/images/share-twitter.png", ContentType(mime::IMAGE_PNG)))
            }
            (&Method::Get, "/facebook-ads/index.js") => {
                Either::B(self.file("public/dist/index.js", ContentType(mime::TEXT_JAVASCRIPT)))
            }
            (&Method::Get, "/facebook-ads/index.js.map") => {
                Either::B(self.file("public/dist/index.js.map", ContentType::json()))
            }
            (&Method::Get, "/facebook-ads/beacons.js") => {
                Either::B(self.file("public/dist/beacons.js", ContentType(mime::TEXT_JAVASCRIPT)))
            }
            (&Method::Get, "/facebook-ads/beacons.js.map") => {
                Either::B(self.file("public/dist/beacons.js.map", ContentType::json()))
            }
            (&Method::Get, "/facebook-ads/styles.css") => {
                Either::B(self.file("public/dist/styles.css", ContentType(mime::TEXT_CSS)))
            }
            (&Method::Get, "/facebook-ads/styles.css.map") => {
                Either::B(self.file("public/dist/styles.css.map", ContentType::json()))
            }
            (&Method::Get, "/facebook-ads/locales/en/translation.json") => {
                Either::B(self.file("public/locales/en/translation.json", ContentType::json()))
            }
            (&Method::Get, "/facebook-ads/locales/de/translation.json") => {
                Either::B(self.file("public/locales/de/translation.json", ContentType::json()))
            }
            (&Method::Get, "/facebook-ads/stream") => Either::B(self.stream()),
            (&Method::Post, "/facebook-ads/ads") => {
                Either::B(self.lang(req, |req, lang| self.process(req, lang)))
            }
            (&Method::Get, "/facebook-ads/heartbeat") => {
                Either::A(future::ok(Response::new().with_status(StatusCode::Ok)))
            }
            (&Method::Get, "/facebook-ads/advertisers") => {
                // TODO: route these in the restful routing area.
                Either::B(self.lang(req, |req, lang| self.advertisers(req, lang, None)))
            }
            (&Method::Get, "/facebook-ads/segments") => {
                // TODO: route these in the restful routing area.
                Either::B(self.lang(req, |req, lang| self.segments(req, lang, None)))
            }

            (&Method::Get, "/facebook-ads/recent_advertisers") => {
                // TODO: route these in the restful routing area.
                Either::B(self.lang(req, |req, lang| {
                    self.advertisers(req, lang, Some(String::from("1 Month")))
                }))
            }
            (&Method::Get, "/facebook-ads/recent_segments") => {
                // TODO: route these in the restful routing area.
                Either::B(self.lang(req, |req, lang| {
                    self.segments(req, lang, Some(String::from("1 Month")))
                }))
            }
            (&Method::Get, "/facebook-ads") => {
                // redirect no-slash to slash
                let mut response = Response::new().with_header(Location::new("/facebook-ads/"));
                Either::A(future::ok(response.with_status(StatusCode::Found)))
            }
            // Restful-ish routing.
            (&Method::Get, _) => {
                let restful = RegexSet::new(&[
                    // rudimentary routing. ORDER MATTERS. And we're using the index of these as
                    // the key for match below.
                    r"^/facebook-ads/$",
                    r"^/facebook-ads/admin/?(.*)?$",
                    r"^/facebook-ads/ads/?(\d+)?$",
                    r"^/facebook-ads/ad/?(\d+)?$",
                    r"^/facebook-ads/ad/",
                    r"^/facebook-ads/yghelp",
                ]).unwrap();
                // generic restful routing regex for distinguishing subroutes at the collection
                // and those at a specific element.  I'm sure I will understand
                // why I needed to call to_owned() here better later, but for
                // now, this is how to avoid borrowing-related issues.
                let collection = Regex::new(r"^/facebook-ads/(?:[^/]+)/?(\d+)$").unwrap();
                let not_found = Either::A(future::ok(
                    Response::new().with_status(StatusCode::NotFound),
                ));
                let my_path = req.path().to_owned();
                let rest_matches: Vec<usize> = restful.matches(&my_path).into_iter().collect();
                match rest_matches.get(0) {
                    None => not_found,
                    // these indices match to the indices of `restful` above.
                    Some(&0) => Either::B(self.file("public/index.html", ContentType::html())),
                    Some(&1) => Either::B(self.file("public/admin.html", ContentType::html())),
                    Some(&2) => {
                        // api
                        match collection.captures(&my_path) {
                            Some(id) => Either::B(self.lang(req, |req, lang| {
                                let id = String::from(id.get(1).unwrap().as_str());
                                info!("Getting {}", id);
                                self.ad(req, lang, id)
                            })),
                            None => Either::B(self.lang(req, |req, lang| self.search(req, lang))),
                        }
                    }
                    Some(&3) => Either::B(self.file("public/ad.html", ContentType::html())), /* this is the permalinks! */
                    Some(&4) => Either::B(self.file("public/ad.html", ContentType::html())), /* this is the permalinks' fake og:ur! */
                    Some(&5) => Either::B(self.file("public/index.html", ContentType::html())), /* this is yghelp */
                    Some(&_) => not_found,
                }
            }
            _ => Either::A(future::ok(
                Response::new().with_status(StatusCode::NotFound),
            )),
        }
    }
}

// this lets us use variables many times by cloning them
macro_rules! spawn_clone {
    ($pool:ident; $($n:ident),+; $body:expr) => ({
        {
            $(let $n = $n.clone();)+
            $pool.spawn_fn(move || $body)
        }
    });
}

// build out a json response
fn json<T: Serialize + Debug>(thing: &T) -> Response {
    serde_json::to_string(thing)
        .map(|serialized| {
            Response::new()
                .with_header(ContentLength(serialized.len() as u64))
                .with_header(Vary::Items(vec![
                    Ascii::new("Accept-Language".to_owned()),
                    Ascii::new("Content-Type".to_owned()),
                ]))
                .with_header(ContentType::json())
                .with_header(AccessControlAllowOrigin::Any)
                .with_body(serialized)
        })
        .map_err(|e| warn!("Couldn't serialize {:?} {:?}", thing, e))
        .unwrap_or_else(|_| Response::new().with_status(StatusCode::InternalServerError))
}

type ResponseFuture = Box<Future<Item = Response, Error = hyper::Error>>;
impl AdServer {
    fn lang<F>(&self, req: Request, callback: F) -> ResponseFuture
    where
        F: Fn(Request, String) -> ResponseFuture,
    {
        let bad = Box::new(future::ok(
            Response::new().with_status(StatusCode::BadRequest),
        ));

        let langs = req.headers()
            .get::<AcceptLanguage>()
            .and_then(|langs| Some(langs.clone()));

        if let Some(langs) = langs {
            if langs.len() == 0 {
                return bad;
            }
            let languages = langs.to_owned();
            let lang = languages
                .iter()
                .find(|quality| quality.item.language.is_some() && quality.item.region.is_some())
                .map(|l| {
                    l.clone().item.language.unwrap_or_default() + "-"
                        + &l.clone().item.region.unwrap_or_default().to_uppercase()
                })
                .unwrap_or_else(|| String::from("en-US"));
            callback(req, lang)
        } else {
            bad
        }
    }

    fn file_bytes(&self, path: &str, content_type: ContentType) -> ResponseFuture {
        let pool = self.pool.clone();
        let path = path.to_string();
        let future = pool.spawn_fn(move || {
            if let Ok(mut file) = File::open(path) {
                let mut buf = Vec::new();
                if let Ok(size) = file.read_to_end(&mut buf) {
                    return Ok(Response::new()
                        .with_header(ContentLength(size as u64))
                        .with_header(content_type)
                        .with_body(buf));
                }
            }
            Ok(Response::new().with_status(StatusCode::NotFound))
        });
        Box::new(future)
    }


    // Responders
    fn file(&self, path: &str, content_type: ContentType) -> ResponseFuture {
        info!("Getting file {:?}", path);
        let pool = self.pool.clone();
        let path = path.to_string();
        let future = pool.spawn_fn(move || {
            if let Ok(mut file) = File::open(path) {
                let mut buf = String::new();
                if let Ok(size) = file.read_to_string(&mut buf) {
                    return Ok(Response::new()
                        .with_header(ContentLength(size as u64))
                        .with_header(content_type)
                        .with_body(buf));
                }
            }
            Ok(Response::new().with_status(StatusCode::NotFound))
        });
        Box::new(future)
    }

    fn ad(&self, _req: Request, lang: String, adid: String) -> ResponseFuture {
        let db_pool = self.db_pool.clone();
        let pool = self.pool.clone();
        let ad = spawn_clone!(pool; lang, db_pool; Ad::find(&lang, &db_pool, adid));
        let future = ad.map(|ad_option| match ad_option {
            None => Response::new().with_status(StatusCode::NotFound),
            Some(ad) => json(&ad),
        }).map_err(|e| hyper::Error::Io(StdIoError::new(StdIoErrorKind::Other, e.description())));
        Box::new(future)
    }

    fn advertisers(&self, _req: Request, lang: String, time: Option<String>) -> ResponseFuture {
        let pool = self.pool.clone();
        let lang = lang.clone();
        let db_pool = self.db_pool.clone();
        let future = pool.spawn_fn(move || {
            Advertisers::get(&lang, &Some(1000), time, &db_pool)
                .map(|advertisers: Vec<Advertisers>| json(&advertisers))
        }).map_err(|e| hyper::Error::Io(StdIoError::new(StdIoErrorKind::Other, e.description())));
        Box::new(future)
    }

    fn segments(&self, _req: Request, lang: String, time: Option<String>) -> ResponseFuture {
        let pool = self.pool.clone();
        let lang = lang.clone();
        let db_pool = self.db_pool.clone();
        let future = pool.spawn_fn(move || {
            Segments::get(&lang, &Some(1000), time, &db_pool)
                .map(|segments: Vec<Segments>| json(&segments))
        }).map_err(|e| hyper::Error::Io(StdIoError::new(StdIoErrorKind::Other, e.description())));
        Box::new(future)
    }

    fn search(&self, req: Request, lang: String) -> ResponseFuture {
        let options = req.query()
            .map(|q| {
                form_urlencoded::parse(q.as_bytes())
                        .into_owned()
                        .filter(|pair| {
                            pair.0 == "search" || pair.0 == "page" || pair.0 == "targets"
                                || pair.0 == "advertisers"
                                || pair.0 == "entities"
                                || pair.0 == "id"
                        })
                        // transforms the Vector of tuples into a HashMap.
                        .collect::<HashMap<_, _>>()
            })
            .unwrap_or_default();

        let db_pool = self.db_pool.clone();
        let pool = self.pool.clone();
        let ads = spawn_clone!(pool; lang, db_pool, options;
                                        Ad::search(&lang, &db_pool, &options));
        let targets = spawn_clone!(pool; lang, db_pool, options;
                                            Targets::search(&lang, &db_pool, &options));
        let entities = spawn_clone!(pool; lang, db_pool, options;
                                  Entities::search(&lang, &db_pool, &options));
        let advertisers = spawn_clone!(pool; lang, db_pool, options;
                                  Advertisers::search(&lang, &db_pool, &options));
        let total = spawn_clone!(pool; lang, db_pool, options;
                                          Ad::search_total(&lang, &db_pool, &options));

        let future = ads.join5(targets, entities, advertisers, total)
            .map(|(ads, targeting, entities, advertisers, total)| {
                json(&ApiResponse {
                    ads: ads,
                    targets: targeting,
                    entities: entities,
                    advertisers: advertisers,
                    total: total,
                })
            })
            .map_err(|e| hyper::Error::Io(StdIoError::new(StdIoErrorKind::Other, e.description())));
        Box::new(future)
    }

    // Beware! This function assumes that we have a caching proxy in front of our
    // web server, otherwise we're making a new connection on every request.
    fn stream(&self) -> ResponseFuture {
        let handle = self.handle.clone();
        let url = Url::parse(&self.database_url.clone()).unwrap();
        let database_url = url.as_str();

        let notifications = Connection::connect(database_url, TlsMode::None, &self.handle.clone())
            .then(|c| c.unwrap().batch_execute("listen ad_update"))
            .map(move |c| {
                let timer = Timer::default()
                    .interval(Duration::from_millis(1000))
                    .map(|_| Ok(Chunk::from("event: ping\n\n")))
                    .map_err(|_| unimplemented!());
                let notifications = c.notifications()
                    .map(|n| Ok(Chunk::from(n.payload)))
                    .map_err(|_| unimplemented!())
                    .select(timer);
                let (sender, body) = Body::pair();
                let resp = Response::new()
                    .with_header(ContentType("text/event-stream".parse().unwrap()))
                    .with_header(CacheControl(vec![
                        CacheDirective::Public,
                        CacheDirective::MaxAge(29),
                    ]))
                    .with_header(HttpConnection::keep_alive())
                    .with_body(body);
                handle.spawn(sender.send_all(notifications).map(|_| ()).map_err(|_| ()));
                resp
            })
            .map_err(|(e, _)| {
                hyper::Error::Io(StdIoError::new(StdIoErrorKind::Other, e.description()))
            });

        Box::new(notifications)
    }

    fn process(&self, req: Request, lang: String) -> ResponseFuture {
        let db_pool = self.db_pool.clone();
        let pool = self.pool.clone();
        let image_pool = self.pool.clone();
        let image_db = self.db_pool.clone();
        let handle = self.handle.clone();
        let client = self.client.clone();
        let future = req.body()
            .concat2()
            .then(move |msg| {
                pool.spawn_fn(move || {
                    AdServer::save(
                        msg.map_err(|e| Error::with_chain(e, "Can't get body")),
                        &db_pool,
                        lang,
                    )
                })
            })
            .and_then(move |ads| {
                for ad in ads {
                    handle.spawn(ad.grab_and_store(client.clone(), &image_db, &image_pool.clone()))
                }

                Ok(Response::new())
            })
            .then(|r| match r {
                Ok(r) => Ok(r),
                Err(e) => {
                    warn!("{:?}", e);
                    Ok(Response::new().with_status(StatusCode::BadRequest))
                }
            });
        Box::new(future)
    }

    fn save(
        msg: Result<Chunk>,
        db_pool: &Pool<ConnectionManager<PgConnection>>,
        lang: String,
    ) -> Result<Vec<Ad>> {
        let bytes = msg?;
        let string = String::from_utf8(bytes.to_vec())?;
        let posts: Vec<AdPost> = serde_json::from_str(&string)?;
        let ads = posts.iter().map(move |post| {
            let ad = NewAd::new(post, &lang)?.save(db_pool)?;
            Ok(ad)
        });
        ads.collect::<Result<Vec<Ad>>>()
    }

    // for suppressin' ads.
    fn mark(&self, req: Request) -> ResponseFuture {
        let db_pool = self.db_pool.clone();
        let pool = self.pool.clone();
        let future = req.body()
            .concat2()
            .then(move |msg| {
                pool.spawn_fn(move || {
                    msg.and_then(|bytes| {
                        String::from_utf8(bytes.to_vec()).map_err(|_| hyper::Error::Timeout)
                    }).and_then(|id| {
                            Ad::suppress(id, &db_pool).map_err(|_| hyper::Error::Timeout)
                        })
                })
            })
            .then(|r| match r {
                Ok(_) => Ok(Response::new().with_status(StatusCode::Ok)),
                Err(e) => {
                    warn!("{:?}", e);
                    Ok(Response::new().with_status(StatusCode::BadRequest))
                }
            });
        Box::new(future)
    }

    pub fn new(handle: Handle, database_url: String) -> AdServer {
        dotenv().ok();
        let manager = ConnectionManager::<PgConnection>::new(database_url.clone());
        let db_pool = Pool::builder()
            .build(manager)
            .expect("Failed to create pool.");
        let pool = CpuPool::new_num_cpus();
        let connector = HttpsConnector::new(4, &handle).expect("Couldn't build HttpsConnector");
        let client = Client::configure()
            .connector(connector)
            .build(&handle.clone());
        AdServer {
            pool: pool,
            db_pool: db_pool,
            handle: handle,
            client: client,
            database_url: database_url,
        }
    }

    pub fn start() {
        dotenv().ok();
        start_logging();

        if let Ok(root) = env::var("ROOT") {
            env::set_current_dir(&root).expect(&format!("Couldn't change directory to {}", root));
        }
        let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set.");

        let addr = env::var("HOST")
            .expect("HOST must be set")
            .parse()
            .expect("Error parsing HOST");

        let mut core = Core::new().unwrap();
        let handle = core.handle();
        let ad_server = AdServer::new(handle.clone(), database_url);
        // from: https://hyper.rs/guides/server/response_strategies/
        let server_handle = handle.clone();
        let server = Http::new()
            .serve_addr_handle(&addr, &server_handle, move || Ok(ad_server.clone()))
            .unwrap();
        let h2 = handle.clone();
        server_handle.spawn(
            server
                .for_each(move |conn| {
                    h2.spawn(
                        conn.map(|_| ())
                            .map_err(|err| warn!("server error: {:?}", err)),
                    );
                    Ok(())
                })
                .map_err(|_| ()),
        );
        println!("Listening on http://{} with 1 thread.", addr);
        core.run(future::empty::<(), ()>()).unwrap();
    }
}
