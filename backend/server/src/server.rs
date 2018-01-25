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
use hyper::Headers;
use hyper::header::{AcceptLanguage, AccessControlAllowOrigin, Authorization, Bearer, CacheControl,
                    CacheDirective, Connection as HttpConnection, ContentLength, ContentType, Vary};
use hyper::mime;
use hyper_tls::HttpsConnector;
use jsonwebtoken::{decode, Validation};
use models::{Ad, Advertisers, Aggregate, Entities, NewAd, Targets};
use r2d2::Pool;
use regex::Regex;
use start_logging;
use serde_json;
use std::collections::HashMap;
use std::env;
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

pub struct AdServer {
    db_pool: Pool<ConnectionManager<PgConnection>>,
    pool: CpuPool,
    handle: Handle,
    client: Client<HttpsConnector<HttpConnector>>,
    password: String,
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

#[derive(Serialize)]
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
        let restfulre = Regex::new(r"^/facebook-ads/ads/?(\d+)?$").unwrap();

        match (req.method(), req.path()) {
            (&Method::Post, "/facebook-ads/login") => Either::B(self.auth(req, |_| {
                Box::new(future::ok(Response::new().with_status(StatusCode::Ok)))
            })),
            // Admin
            (&Method::Get, "/facebook-ads/admin") => {
                Either::B(self.get_file("public/admin.html", ContentType::html()))
            }
            (&Method::Get, "/facebook-ads/admin.js") => Either::B(self.get_file(
                "public/dist/admin.js",
                ContentType(mime::TEXT_JAVASCRIPT),
            )),
            (&Method::Get, "/facebook-ads/admin.js.map") => Either::B(self.get_file(
                "public/dist/admin.js.map",
                ContentType::json(),
            )),
            (&Method::Get, "/facebook-ads/admin/styles.css") => Either::B(self.get_file(
                "public/css/admin/styles.css",
                ContentType(mime::TEXT_CSS),
            )),
            (&Method::Post, "/facebook-ads/admin/ads") => {
                Either::B(self.auth(req, |request| self.mark_ad(request)))
            }
            // Public
            (&Method::Get, "/facebook-ads/") => {
                Either::B(self.get_file("public/index.html", ContentType::html()))
            }
            (&Method::Get, "/facebook-ads/index.js") => Either::B(self.get_file(
                "public/dist/index.js",
                ContentType(mime::TEXT_JAVASCRIPT),
            )),
            (&Method::Get, "/facebook-ads/index.js.map") => Either::B(self.get_file(
                "public/dist/index.js.map",
                ContentType::json(),
            )),
            (&Method::Get, "/facebook-ads/beacons.js") => Either::B(self.get_file(
                "public/dist/beacons.js",
                ContentType(mime::TEXT_JAVASCRIPT),
            )),
            (&Method::Get, "/facebook-ads/beacons.js.map") => Either::B(self.get_file(
                "public/dist/beacons.js.map",
                ContentType::json(),
            )),
            (&Method::Get, "/facebook-ads/styles.css") => Either::B(self.get_file(
                "public/dist/styles.css",
                ContentType(mime::TEXT_CSS),
            )),
            (&Method::Get, "/facebook-ads/styles.css.map") => Either::B(self.get_file(
                "public/dist/styles.css.map",
                ContentType::json(),
            )),
            (&Method::Get, "/facebook-ads/locales/en/translation.json") => Either::B(
                self.get_file("public/locales/en/translation.json", ContentType::json()),
            ),
            (&Method::Get, "/facebook-ads/locales/de/translation.json") => Either::B(
                self.get_file("public/locales/de/translation.json", ContentType::json()),
            ),
            (&Method::Get, "/facebook-ads/stream") => Either::B(self.stream_ads()),
            (&Method::Post, "/facebook-ads/ads") => Either::B(self.process_ads(req)),
            (&Method::Get, "/facebook-ads/heartbeat") => {
                Either::A(future::ok(Response::new().with_status(StatusCode::Ok)))
            }
            // I'm sure I will understand why I needed to call to_owned() here better later,
            // but for now, this is how to avoid borrowing-related issues.
            (&Method::Get, _) => match restfulre.captures(&req.path().to_owned()){ 
                Some(ads_match) => {
                    match ads_match.get(1) {
                        Some(id_match) => {
                            match id_match.as_str() {
                                "" | "/" => {
                                    Either::B(self.get_ads(req))
                                },
                                _ => {
                                    Either::B(self.get_ad(id_match.as_str().into()))
                                }
                            }
                        },
                        None => {
                            Either::B(self.get_ads(req))
                        }
                    }
                }
                None => Either::A(future::ok(Response::new().with_status(StatusCode::NotFound)))
            },
            _ => Either::A(future::ok(
                Response::new().with_status(StatusCode::NotFound),
            )),
        }
    }
}

// this lets us use variables many times by cloning them
macro_rules! spawn_with_clone {
    ($pool:ident; $($n:ident),+; $body:expr) => ({
        {
            $(let $n = $n.clone();)+
            $pool.spawn_fn(move || $body)
        }
    });
}

// I'm not happy with the OK OK OKs here, but I can't quite find a Result
// method that works. I should ask on stack overflow or something.
type ResponseFuture = Box<Future<Item = Response, Error = hyper::Error>>;
impl AdServer {
    fn auth<F>(&self, req: Request, callback: F) -> ResponseFuture
    where
        F: Fn(Request) -> ResponseFuture,
    {
        let auth = req.headers()
            .get::<Authorization<Bearer>>()
            .and_then(|token| {
                decode::<Admin>(&token.token, self.password.as_ref(), &Validation::default()).ok()
            });

        if auth.is_some() {
            info!("Login {:?}", auth);
            callback(req)
        } else {
            warn!("Bad Login {:?}", auth);
            Box::new(future::ok(
                Response::new().with_status(StatusCode::Unauthorized),
            ))
        }
    }

    fn get_file(&self, path: &str, content_type: ContentType) -> ResponseFuture {
        info!("Getting file {:?}", path);
        let pool = self.pool.clone();
        let path = path.to_string();
        let future = pool.spawn_fn(move || {
            if let Ok(mut file) = File::open(path) {
                let mut buf = String::new();
                if let Ok(size) = file.read_to_string(&mut buf) {
                    return Ok(
                        Response::new()
                            .with_header(ContentLength(size as u64))
                            .with_header(content_type)
                            .with_body(buf),
                    );
                }
            }
            Ok(Response::new().with_status(StatusCode::NotFound))
        });
        Box::new(future)
    }

    fn get_lang_from_headers(headers: &Headers) -> Option<String> {
        if let Some(langs) = headers.get::<AcceptLanguage>() {
            if langs.len() == 0 {
                return None;
            }
            let languages = langs.to_owned();
            let lang = languages.iter().find(|quality| {
                quality.item.language.is_some() && quality.item.region.is_some()
            });
            if let Some(l) = lang {
                Some(
                    l.clone().item.language.unwrap_or_default() + "-"
                        + &l.clone().item.region.unwrap_or_default().to_uppercase(),
                )
            } else {
                Some(String::from("en-US"))
            }
        } else {
            None
        }
    }

    fn get_ad(&self, adid: String) -> ResponseFuture {
        let db_pool = self.db_pool.clone();
        let pool = self.pool.clone();
        let ad = spawn_with_clone!(pool; db_pool, adid;
                        Ad::get_ad(&db_pool, adid)
                    );
        let future = ad.map(|ad| {
            serde_json::to_string(&ApiResponse {
                ads: vec![ad],
                targets: Vec::<Targets>::new(),
                entities: Vec::<Entities>::new(),
                advertisers: Vec::<Advertisers>::new(),
                total: 1,
            }).map(|serialized| {
                Response::new()
                    .with_header(ContentLength(serialized.len() as u64))
                    .with_header(Vary::Items(vec![Ascii::new("Accept-Language".to_owned())]))
                    .with_header(ContentType::json())
                    .with_header(AccessControlAllowOrigin::Any)
                    .with_body(serialized)
            })
                .unwrap_or(Response::new().with_status(StatusCode::InternalServerError))
        }).map_err(|e| {
            warn!("{:?}", e);
            hyper::Error::Io(StdIoError::new(StdIoErrorKind::Other, e.description()))
        });
        Box::new(future)
    }

    fn get_ads(&self, req: Request) -> ResponseFuture {
        if let Some(lang) = AdServer::get_lang_from_headers(req.headers()) {
            let options = req.query()
                .map(|q| {
                    form_urlencoded::parse(q.as_bytes())
                        .into_owned()
                        .filter(|pair| {
                            pair.0 == "search" || pair.0 == "page" || pair.0 == "targets" ||
                                pair.0 == "advertisers" ||
                                pair.0 == "entities"    || pair.0 == "id"
                        })
                        .collect::<HashMap<_, _>>() // transforms the Vector of tuples into a HashMap.
                })
                .unwrap_or_default();

            let db_pool = self.db_pool.clone();
            let pool = self.pool.clone();
            let ads = spawn_with_clone!(pool; lang, db_pool, options;
                                        Ad::get_ads(&lang, &db_pool, &options));
            let targets = spawn_with_clone!(pool; lang, db_pool, options;
                                            Targets::get(&lang, &db_pool, &options));
            let entities = spawn_with_clone!(pool; lang, db_pool, options;
                                  Entities::get(&lang, &db_pool, &options));
            let advertisers = spawn_with_clone!(pool; lang, db_pool, options;
                                  Advertisers::get(&lang, &db_pool, &options));
            let total = spawn_with_clone!(pool; lang, db_pool, options;
                                          Ad::get_total(&lang, &db_pool, &options));

            let future = ads.join5(targets, entities, advertisers, total)
                .map(|(ads, targeting, entities, advertisers, total)| {
                    serde_json::to_string(&ApiResponse {
                        ads: ads,
                        targets: targeting,
                        entities: entities,
                        advertisers: advertisers,
                        total: total,
                    }).map(|serialized| {
                        Response::new()
                            .with_header(ContentLength(serialized.len() as u64))
                            .with_header(
                                Vary::Items(vec![Ascii::new("Accept-Language".to_owned())]),
                            )
                            .with_header(ContentType::json())
                            .with_header(AccessControlAllowOrigin::Any)
                            .with_body(serialized)
                    })
                        .unwrap_or(Response::new().with_status(StatusCode::InternalServerError))
                })
                .map_err(|e| {
                    warn!("{:?}", e);
                    hyper::Error::Io(StdIoError::new(StdIoErrorKind::Other, e.description()))
                });
            Box::new(future)
        } else {
            Box::new(future::ok(
                Response::new().with_status(StatusCode::BadRequest),
            ))
        }
    }

    // Beware! This function assumes that we have a caching proxy in front of our
    // web server, otherwise we're making a new connection on every request.
    fn stream_ads(&self) -> ResponseFuture {
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
                    .with_header(CacheControl(
                        vec![CacheDirective::Public, CacheDirective::MaxAge(29)],
                    ))
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

    fn process_ads(&self, req: Request) -> ResponseFuture {
        let db_pool = self.db_pool.clone();
        let pool = self.pool.clone();
        let image_pool = self.pool.clone();
        let image_db = self.db_pool.clone();
        let handle = self.handle.clone();
        let client = self.client.clone();
        let maybe_lang = AdServer::get_lang_from_headers(req.headers());
        if !maybe_lang.is_some() {
            return Box::new(future::ok(
                Response::new().with_status(StatusCode::BadRequest),
            ));
        };
        let lang = maybe_lang.unwrap();

        let future = req.body()
            .concat2()
            .then(move |msg| {
                pool.spawn_fn(move || {
                    AdServer::save_ads(
                        msg.map_err(|e| Error::with_chain(e, "Can't get body")),
                        &db_pool,
                        lang,
                    )
                })
            })
            .and_then(move |ads| {
                for ad in ads {
                    handle.spawn(ad.grab_and_store(
                        client.clone(),
                        &image_db,
                        &image_pool.clone(),
                    ))
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

    fn save_ads(
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
    fn mark_ad(&self, req: Request) -> ResponseFuture {
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

    pub fn start() {
        dotenv().ok();
        start_logging();

        if let Ok(root) = env::var("ROOT") {
            env::set_current_dir(&root).expect(&format!("Couldn't change directory to {}", root));
        }

        let addr = env::var("HOST")
            .expect("HOST must be set")
            .parse()
            .expect("Error parsing HOST");

        let admin_password = env::var("ADMIN_PASSWORD").expect("ADMIN_PASSWORD must be set.");
        let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set.");
        let manager = ConnectionManager::<PgConnection>::new(database_url.clone());
        let db_pool = Pool::builder()
            .build(manager)
            .expect("Failed to create pool.");
        let pool = CpuPool::new_num_cpus();

        let mut core = Core::new().unwrap();
        let handle = core.handle();
        let connector =
            HttpsConnector::new(4, &core.handle()).expect("Couldn't build HttpsConnector");
        let client = Client::configure()
            .connector(connector)
            .build(&core.handle());
        // from: https://hyper.rs/guides/server/response_strategies/
        let server_handle = handle.clone();
        let server = Http::new().serve_addr_handle(&addr, &server_handle, move || {
            Ok(AdServer {
                pool: pool.clone(),
                db_pool: db_pool.clone(),
                handle: handle.clone(),
                client: client.clone(),
                password: admin_password.clone(),
                database_url: database_url.clone(),
            })
        }).unwrap();
        let h2 = server_handle.clone();
        server_handle.spawn(server.for_each(move |conn| {
            h2.spawn(conn.map(|_| ()).map_err(|err| warn!("server error: {:?}", err)));
            Ok(())
        }).map_err(|_| ()));
        println!("Listening on http://{} with 1 thread.", addr);
        core.run(future::empty::<(), ()>()).unwrap();
    }
}
