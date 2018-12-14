use chrono::offset::Utc;
use chrono::DateTime;
use diesel;
use diesel::dsl::sql;
use diesel::pg::Pg;
use diesel::pg::PgConnection;
use diesel::prelude::*;
use diesel::r2d2::ConnectionManager;
use diesel::sql_types::{BigInt, Bool, Text};
use diesel_full_text_search::*;
use errors::*;
use futures::future;
use futures::future::Either;
use futures::{stream, Future, Stream};
use futures_cpupool::CpuPool;
use hyper::client::HttpConnector;
use hyper::{Body, Client, Uri};
use hyper_tls::HttpsConnector;
use kuchiki;
use kuchiki::iter::{Descendants, Elements, Select};
use kuchiki::traits::*;
use r2d2::Pool;
use rusoto_core::Region;
use rusoto_s3::{PutObjectRequest, S3, S3Client};
use schema::ads;
use schema::ads::BoxedQuery;
use serde_json;
use serde_json::Value;
use server::AdPost;
use std::collections::{HashMap, HashSet};
use std::cmp;
use targeting_parser::{collect_advertiser, collect_targeting, Targeting};
use url::{ParseError, Url};

const DEFAULT_S3_BUCKET_NAME : &'static str = "pp-facebook-ads";
const S3_BUCKET_NAME: Option<&'static str> = option_env!("S3_BUCKET_NAME");

pub fn document_select(
    document: &kuchiki::NodeRef,
    selector: &str,
) -> Result<Select<Elements<Descendants>>> {
    document
        .select(selector)
        .map_err(|_| ErrorKind::HTML(format!("Selector compile error {}", selector)).into())
}

pub fn get_title(document: &kuchiki::NodeRef) -> Result<String> {
    document_select(document, "h5 a, h6 a, strong, span.fsl")?
        .nth(0)
        .and_then(|a| Some(a.text_contents()))
        .ok_or_else(|| "Couldn't find title.".into())
}

fn get_image(document: &kuchiki::NodeRef) -> Result<String> {
    document_select(document, "img")?
        .nth(0)
        .and_then(|a| {
            a.attributes
                .borrow()
                .get("src")
                .and_then(|src| Some(src.to_string()))
        })
        .ok_or_else(|| "Couldn't find images.".into())
}

pub fn get_message(document: &kuchiki::NodeRef) -> Result<String> {
    let selectors = vec![".userContent p", "div.mbs", "span"];
    let iters = selectors
        .iter()
        .map(|s| document_select(document, s))
        .flat_map(|a| a);

    iters
        .map(|i| i.fold(String::new(), |m, a| m + &a.as_node().to_string()))
        .filter(|i| !i.is_empty())
        .nth(0)
        .ok_or_else(|| "Couldn't find message.".into())
}

pub fn get_paid_for_by(document: &kuchiki::NodeRef) -> Option<String> {
    document_select(document, "._5pcq ._3nlk").ok()
        .and_then(|mut elems| 
            elems.nth(1).and_then(
                |elem| Some(String::new() + &elem.as_node().text_contents().to_string())
            )
        ) 
}

// pub fn get_paid_for_by(document: &kuchiki::NodeRef) -> Result<String> {
//     document_select(document, "._5pcq ._3nlk")
//         .and_then(|mut elems| 
//             elems.nth(1).ok_or_else(|| "can't find Paid for by".into())
//         .and_then(
//                 |elem| Ok(String::new() + &elem.as_node().text_contents().to_string())
//             )
//         ) 
// }

// Only available in timeline ads
pub fn get_author_link(
    document: &kuchiki::NodeRef,
) -> Result<kuchiki::NodeDataRef<kuchiki::ElementData>> {
    document_select(document, ".fwb a")?
        .nth(0)
        .ok_or_else(|| "Couldn't find advertiser link".into())
}

fn get_images(document: &kuchiki::NodeRef) -> Result<Vec<String>> {
    let select = document_select(document, "img")?;
    Ok(select
        .skip(1)
        .map(|a| {
            a.attributes
                .borrow()
                .get("src")
                .and_then(|s| Some(s.to_string()))
        })
        .filter(|s| s.is_some())
        .map(|s| s.unwrap())
        .collect::<Vec<String>>())
}

fn get_real_image_uri(uri: Uri) -> Uri {
    let url = uri.to_string().parse::<Url>();
    let real_url = match url {
        Err(ParseError::RelativeUrlWithoutBase) => {
            let base_url = Url::parse("https://facebook.com");
            base_url.unwrap().join(&uri.to_string()).unwrap()
        }
        Err(e) => panic!(e.to_string()),
        Ok(u) => u, // basically just unwrap
    };
    let query_map: HashMap<_, _> = real_url.query_pairs().into_owned().collect();
    query_map.get("url")
        .map(|u| u.parse::<Uri>()) // Option<Result>
        .unwrap_or_else(|| Ok(uri.clone())) // Result
        .unwrap_or(uri) // Uri
}

#[derive(AsChangeset, Debug)]
#[table_name = "ads"]
pub struct Images {
    thumbnail: Option<String>,
    images: Vec<String>,
    title: String,
    message: String,
    html: String,
}
impl Images {
    fn from_ad(ad: &Ad, images: &[Uri]) -> Result<Images> {
        let s3_endpoint : &str = &("https://".to_owned() + S3_BUCKET_NAME.unwrap_or(DEFAULT_S3_BUCKET_NAME) + &".s3.amazonaws.com/".to_owned());
        let thumb = images
            .iter()
            .filter(|i| ad.thumbnail.contains(i.path()))
            .map(|i| s3_endpoint.to_string() + i.path().trim_left_matches('/'))
            .nth(0);
        let mut rest = images.to_owned();
        if let Some(thumb) = thumb.clone() {
            rest.retain(|x| !thumb.contains(x.path()))
        };

        let collection = rest.iter()
            .filter(|i| ad.images.iter().any(|a| a.contains(i.path())))
            .map(|i| s3_endpoint.to_string() + i.path().trim_left_matches('/'))
            .collect::<Vec<String>>();
        let document = kuchiki::parse_html().one(ad.html.clone());
        for a in document_select(&document, "img")? {
            if let Some(x) = a.attributes.borrow_mut().get_mut("src") {
                if let Ok(u) = x.parse::<Uri>() {
                    if let Some(i) = images
                        .iter()
                        .find(|i| i.path() == get_real_image_uri(u.clone()).path())
                    {
                        *x = s3_endpoint.to_string() + i.path().trim_left_matches('/');
                    } else {
                        *x = "".to_string();
                    }
                }
            };
        }

        let title = get_title(&document)?;
        let message = get_message(&document)?;
        Ok(Images {
            thumbnail: thumb,
            images: collection,
            title: title,
            html: document_select(&document, "div")?
                .nth(0)
                .ok_or("Couldn't find a div in the html")?
                .as_node()
                .to_string(),
            message: message,
        })
    }
}

macro_rules! agg {
    ($query:expr) => {{
        $query.select((
                    sql::<BigInt>("count(*) as count"),
                    sql::<Text>(Self::column()),
                ))
                .group_by(sql::<Text>(Self::field()))
                /* Ideally we'd have a .having() here, but it's not implemented yet in Diesel. */
                .order(sql::<BigInt>("count desc"))
                .filter(sql::<Text>(Self::null_check()).is_not_null())
    }};
}

pub trait Aggregate<T: Queryable<(BigInt, Text), Pg>> {
    fn field() -> &'static str {
        Self::column()
    }

    fn column() -> &'static str;

    fn null_check() -> &'static str {
        Self::field()
    }

    fn get(
        language: &str,
        limit: &Option<i64>,
        interval: Option<String>,
        conn: &Pool<ConnectionManager<PgConnection>>,
    ) -> Result<Vec<T>> {
        // uncomment to use PgInterval
        // use diesel::dsl::now;
        // use diesel::pg::expression::extensions::IntervalDsl;
        // use schema::ads::columns::created_at;
        let connection = conn.get()?;

        let query = agg!(match interval {
            Some(interv) => {
                let mut interval_str = String::from("created_at > NOW() - interval '");
                interval_str.push_str(&interv);
                interval_str.push_str("'");
                Ad::scoped(language).filter(sql::<Bool>(&interval_str))
            }
            None => Ad::scoped(language),
        }).limit(limit.unwrap_or(20));
        Ok(query.load::<T>(&*connection)?)
    }

    fn search(
        language: &str,
        conn: &Pool<ConnectionManager<PgConnection>>,
        options: &HashMap<String, String>,
    ) -> Result<Vec<T>> {
        let connection = conn.get()?;
        let query = agg!(Ad::search_query(language, options)).limit(20);
        Ok(query.load::<T>(&*connection)?)
    }
}

#[derive(Queryable, Serialize, Deserialize, Debug, Clone)]
pub struct Targets {
    pub count: i64,
    pub target: String,
}

impl<T> Aggregate<T> for Targets
where
    T: Queryable<(BigInt, Text), Pg>,
{
    fn field() -> &'static str {
        "target"
    }

    fn column() -> &'static str {
        "jsonb_array_elements(targets)->>'target' as target"
    }

    fn null_check() -> &'static str {
        "targets"
    }
}

#[derive(Queryable, Serialize, Deserialize, Debug, Clone)]
pub struct Segments {
    pub count: i64,
    pub segment: String,
}

impl<T> Aggregate<T> for Segments
where
    T: Queryable<(BigInt, Text), Pg>,
{
    fn field() -> &'static str {
        "segment"
    }

    fn column() -> &'static str {
        "('{\"segment\": \"(n/a)\"}' || jsonb_array_elements(targets)->>'segment') as segment"
    }

    fn null_check() -> &'static str {
        "targets"
    }
}

#[derive(Serialize, Deserialize)]
pub struct EntityFilter {
    entity: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    entity_type: Option<String>,
}

#[derive(Queryable, Serialize, Deserialize, Debug, Clone)]
pub struct Entities {
    pub count: i64,
    pub entity: String,
}

impl<T> Aggregate<T> for Entities
where
    T: Queryable<(BigInt, Text), Pg>,
{
    fn field() -> &'static str {
        "entity"
    }

    fn column() -> &'static str {
        "jsonb_array_elements(entities)->>'entity' as entity"
    }

    fn null_check() -> &'static str {
        "entities"
    }
}

#[derive(Queryable, Serialize, Deserialize, Debug, Clone)]
pub struct Advertisers {
    pub count: i64,
    pub advertiser: String,
}

impl<T> Aggregate<T> for Advertisers
where
    T: Queryable<(BigInt, Text), Pg>,
{
    fn column() -> &'static str {
        "advertiser"
    }
}

#[derive(Serialize, Queryable, Debug, Clone)]
pub struct Ad {
    pub id: String,
    pub html: String,
    pub political: i32,
    pub not_political: i32,
    pub title: String,
    pub message: String,
    pub thumbnail: String,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub lang: String,
    pub images: Vec<String>,
    pub impressions: i32,
    pub political_probability: f64,
    pub targeting: Option<String>,
    #[serde(skip_serializing)]
    pub suppressed: bool,
    pub targets: Option<Value>,
    pub advertiser: Option<String>,
    pub entities: Option<Value>,
    pub page: Option<String>,
    pub lower_page: Option<String>,
    pub paid_for_by: Option<String>,
    pub targetings: Option<Vec<String>>,
    pub targetedness: Option<i32>
}
// Define our special functions for searching
sql_function!(to_englishtsvector, to_englishtsvector_t, (x: Text) -> TsVector);
sql_function!(to_germantsvector, to_germantsvector_t, (x: Text) -> TsVector);
sql_function!(to_englishtsquery, to_englishtsquery_t, (x: Text) -> TsQuery);
sql_function!(to_germantsquery, to_germantsquery_t, (x: Text) -> TsQuery);

impl Ad {
    // This will asynchronously save the images to s3 we may very well end up
    // dropping images, but I can't see any way around it right now. Also we
    // should think about splitting this up, but I'm fine -- if a little
    // embarassed about it -- right now. This function swallows errors, and
    // there's a chance we'll end up with no images at the end, but I think we
    // can handle that in the extension's UI.
    pub fn grab_and_store(
        &self,
        client: Client<HttpsConnector<HttpConnector>, Body>,
        db: &Pool<ConnectionManager<PgConnection>>,
        pool: &CpuPool,
    ) -> Box<Future<Item = (), Error = ()>> {
        let ad = self.clone();
        let pool_db = pool.clone();
        let db = db.clone();
        let s3hostname = S3_BUCKET_NAME.unwrap_or(DEFAULT_S3_BUCKET_NAME).to_owned() + &(".s3.amazonaws.com".to_owned());
        let s3hostname2 = s3hostname.clone(); // to circumvent some lifecycle thing I don't have time to fix right now.
        let future = stream::iter_ok(self.image_urls())
            // filter ones we already have in the db and ones we can verify as
            // coming from fb, we don't want to become a malware vector :)
            // currently we redownload images we already have, but ok.
            .filter(move |u| {
                info!("testing {:?}", u.host());
                match u.host() {
                    Some(h) => (h == s3hostname2 || h.ends_with("fbcdn.net")),
                    None => false
                }
            })
            // grab image
            .and_then(move |img| {
                let real_url = get_real_image_uri(img);
                info!("getting {:?}", real_url.path());
                client
                    .get(real_url.clone())
                    .and_then(|res| {
                        res.body().concat2().and_then(|chunk| Ok((chunk, real_url)))
                    })
                    .map_err(|e| Error::with_chain(e, "Could not get image"))
            })
            // upload them to s3
            .and_then(move |tuple| {
                    if tuple.1.host().unwrap_or_default() != s3hostname {
                        let client = S3Client::simple(Region::UsEast1);
                        let req = PutObjectRequest {
                            bucket: S3_BUCKET_NAME.unwrap_or(DEFAULT_S3_BUCKET_NAME).to_string(),
                            key: tuple.1.path().trim_left_matches('/').to_string(),
                            acl: Some("public-read".to_string()),
                            body: Some(tuple.0.to_vec()),
                            ..PutObjectRequest::default()
                        };
                        Either::A(client.put_object(&req)
                            .map_err(|e| { warn!("could not store image {:?}", e); e.into() })
                            .and_then(|_| Ok(tuple.1)))
                    } else {
                        Either::B(future::ok(tuple.1).and_then(|i| Ok(i)))
                    }
            })
            .collect()
            // save the new urls to the database. the images variable will
            // include only those that we've successfully saved to s3, so we
            // have to do a funky merge here.
            .and_then(move |images| {
                let imgs = images.clone();
                pool_db.spawn_fn(move || {
                    use schema::ads::dsl::*;
                    let update = Images::from_ad(&ad, &imgs)?;
                    let connection = db.get()?;
                    diesel::update(ads.find(&ad.id))
                        .set(&update)
                        .execute(&*connection)?;
                    info!("saved {:?}", ad.id);
                    Ok(())
                })
            })
            .or_else(|e| {
                warn!("{:?}", e);
                Ok(())
            });
        Box::new(future)
    }

    pub(self) fn image_urls(&self) -> Vec<Uri> {
        let images = [vec![self.thumbnail.clone()], self.images.clone()];
        images
            .concat()
            .iter()
            .flat_map(|a| a.parse::<Uri>())
            .collect()
    }

    pub fn scoped<'a>(language: &'a str) -> BoxedQuery<'a, Pg> {
        use schema::ads::dsl::*;
        ads.filter(lang.eq(language))
            .filter(political_probability.gt(0.70))
            .filter(suppressed.eq(false))
            .into_boxed()
    }

    pub fn search_total(
        language: &str,
        conn: &Pool<ConnectionManager<PgConnection>>,
        options: &HashMap<String, String>,
    ) -> Result<i64> {
        let connection = conn.get()?;
        let count = Ad::search_query(language, options)
            .count()
            .get_result::<i64>(&*connection)?;
        Ok(count)
    }

    pub fn search_query<'a>(
        language: &'a str,
        options: &'a HashMap<String, String>,
    ) -> BoxedQuery<'a, Pg> {
        use schema::ads::dsl::*;
        let mut query = Ad::scoped(language);

        if let Some(search) = options.get("search") {
            query = match &language[..2] {
                "de" => {
                    query.filter(to_germantsvector(html).matches(to_germantsquery(search.clone())))
                }
                _ => query
                    .filter(to_englishtsvector(html).matches(to_englishtsquery(search.clone()))),
            }
        }
        // TODO: Make these into a function
        if let Some(target) = options.get("targets") {
            let targts: Result<Vec<Targeting>> = serde_json::from_str(target).map_err(|e| e.into());
            if targts.is_ok() {
                let json = serde_json::to_string(&targts.unwrap()).unwrap();
                query = query.filter(sql(&format!("targets @> '{}'", json)));
            }
        }

        if let Some(entity) = options.get("entities") {
            let ents: Result<Vec<EntityFilter>> =
                serde_json::from_str(entity).map_err(|e| e.into());
            if ents.is_ok() {
                let json = serde_json::to_string(&ents.unwrap()).unwrap();
                query = query.filter(sql(&format!("entities @> '{}'", json)));
            }
        }

        if let Some(advertisers) = options.get("advertisers") {
            let adverts: Result<Vec<String>> =
                serde_json::from_str(advertisers).map_err(|e| e.into());
            if adverts.is_ok() {
                query = query.filter(advertiser.eq_any(adverts.unwrap()));
            }
        }

        query
    }

    pub fn search(
        language: &str,
        conn: &Pool<ConnectionManager<PgConnection>>,
        options: &HashMap<String, String>,
    ) -> Result<Vec<Ad>> {
        use schema::ads::dsl::*;
        let connection = conn.get()?;
        let mut query = Ad::search_query(language, options);

        if let Some(p) = options.get("page") {
            let raw_offset = p.parse::<usize>().unwrap_or_default() * 20;
            let offset = if raw_offset > 10_000 {
                10_000
            } else {
                raw_offset
            };
            query = query.offset(offset as i64);
        }

        Ok(query
            .limit(20)
            .order(created_at.desc())
            .load::<Ad>(&*connection)?)
    }

    pub fn find(
        language: &str,
        conn: &Pool<ConnectionManager<PgConnection>>,
        id: String,
    ) -> Result<Option<Ad>> {
        use schema::ads::dsl::id as db_id;
        let connection = conn.get()?;
        let query = Ad::scoped(language);
        info!("Getting from db {}", id);
        // returns a Result with value of Ok(Option(Ad)) OR a Result with value
        // Err(somethin)
        Ok(query
            .filter(db_id.eq(id))
            .limit(1)
            .first::<Ad>(&connection)
            .optional()?)
    }

    pub fn suppress(adid: String, conn: &Pool<ConnectionManager<PgConnection>>) -> Result<()> {
        use schema::ads::dsl::*;
        let connection = conn.get()?;
        {
            warn!("Suppressed {:?}", adid);
        }
        diesel::update(ads.filter(id.eq(adid)))
            .set(suppressed.eq(true))
            .execute(&connection)?;
        Ok(())
    }
}

// the more granularly targeted, the higher the score
pub fn get_targetedness_score(targets: Option<Value>) -> Option<i32> {
    match targets.clone() {
        None => None,
        Some(targets_json) => {
            let mut targets_value = serde_json::to_value(targets_json).unwrap();
            let targs = targets_value.as_array_mut().unwrap();
            let mut keys_existant_so_far = HashSet::new();
            let deduped_targs = targs.iter().
                filter(|elem| {
                    let target : &str = elem.as_object().unwrap().get("target").unwrap().as_str().unwrap();
                    let already_present = keys_existant_so_far.contains(target);
                    keys_existant_so_far.insert(target.clone());  
                    !already_present
                });
            let targetedness : i32 = deduped_targs
                .filter(|elem| elem.as_str().unwrap_or_else(|| "") != "{\"target\": \"Region\", \"segment\": \"the United States\"}") // remove region: US
                .map(|elem| {
                    // println!("{:?}", exlem.as_object().unwrap().get("target").unwrap().as_str().unwrap());
                    let segment = elem.as_object().unwrap().get("segment").map(|s| s.as_str()).unwrap_or_else(|| None);
                    match elem.as_object().unwrap().get("target").unwrap().as_str().unwrap() {
                        "Employer" => 4,
                        "School" => 4,
                        "City" => 3,
                        "Gender" => 3,
                        "Agency" => 3,      // Data Brokers
                        "Interest" => 3,
                        "Segment" => 3,
                        "List" => 3,       // Custom Audience
                        "MinAge" => cmp::min((segment.unwrap().parse::<i32>().unwrap() - 18 ) / 10, 1) ,
                        "MaxAge" => cmp::min((65 - segment.unwrap().parse::<i32>().unwrap() ) / 10, 1),
                        "Retargeting" => match segment.unwrap() {
                            "Near their business" => 3,
                            "people who may be similar to their customers" => 2,   
                            _ => 0
                        }, 
                        "Like" => 1,
                        "State" => 1,
                        "Region" => 1,
                        "Website" => 1,
                        "EngagedWithContent" => 1,
                        "ActivityOnTheFacebookFamily" => 1,
                        "Language" => 0,
                        "Age" => 0,    // covered by minage/maxage
                        _ => 0
                    }
                })
                .sum();
            // TODO:  sort and uniqify target names (so we don't count two instances of City as more targeted than one)
            Some(targetedness)
        }
    }
}

pub fn get_targets(targeting: &Option<String>) -> Option<Value> {
    match *targeting {
        Some(ref targeting) => collect_targeting(targeting)
            .map(|t| serde_json::to_value(t).unwrap())
            .ok(),
        None => None,
    }
}

pub fn get_advertiser(targeting: &Option<String>, document: &kuchiki::NodeRef) -> Option<String> {
    match *targeting {
        Some(ref targeting) => collect_advertiser(targeting)
            .or_else(|| get_author_link(document).map(|a| a.text_contents()).ok()),
        None => get_author_link(document).map(|a| a.text_contents()).ok(),
    }
}

#[derive(Insertable, Debug)]
#[table_name = "ads"]
pub struct NewAd<'a> {
    pub id: &'a str,
    html: &'a str,
    political: i32,
    not_political: i32,

    title: String,
    message: String,
    thumbnail: String,

    lang: &'a str,
    images: Vec<String>,
    impressions: i32,

    targeting: Option<String>,
    pub targets: Option<Value>,
    advertiser: Option<String>,
    page: Option<String>,
    lower_page: Option<String>,
    paid_for_by: Option<String>,
    targetings: Option<Vec<String>>,
    targetedness: Option<i32>
}

impl<'a> NewAd<'a> {
    pub fn new(ad: &'a AdPost, lang: &'a str) -> Result<NewAd<'a>> {
        let document = kuchiki::parse_html().one(ad.html.clone());

        let thumb = get_image(&document)?;
        let images = get_images(&document)?;
        let message = get_message(&document)?;
        let title = get_title(&document)?;
        let paid_for_by = get_paid_for_by(&document);
        let page = get_author_link(&document)
            .ok()
            .and_then(|l| l.attributes.borrow().get("href").map(|i| i.to_string()));
        let parsed_targets = get_targets(&ad.targeting);
        Ok(NewAd {
            id: &ad.id,
            html: &ad.html,
            // we try unwrapping or we chose the false branch in both of these
            // cases to count impressions
            political: if ad.political.unwrap_or(false) { 1 } else { 0 },
            not_political: if !ad.political.unwrap_or(true) { 1 } else { 0 },
            title: title,
            message: message,
            thumbnail: thumb,
            lang: lang,
            images: images,
            impressions: if !ad.political.is_some() { 1 } else { 0 },
            targeting: ad.targeting.clone(),
            targets: parsed_targets.clone(),
            advertiser: get_advertiser(&ad.targeting, &document),
            page: page.clone(),
            lower_page: page.map(|s| s.to_lowercase()),
            paid_for_by: paid_for_by,
            targetings: ad.targeting.clone().map_or(None, |targ| Some(vec![targ])),
            targetedness: get_targetedness_score(parsed_targets)
        })
    }

    pub fn save(&self, pool: &Pool<ConnectionManager<PgConnection>>) -> Result<Ad> {
        use schema::ads;
        use schema::ads::dsl;
        let connection = pool.get()?;
        // increment impressions if this is a background save,
        // otherwise increment political counters
        let saved_ad: Ad = diesel::insert_into(ads::table)
            .values(self)
            .on_conflict(dsl::id)
            .do_update()
            .set(( /* this is what we do if the ad already exists in the DB */
                dsl::political.eq(dsl::political + self.political),
                dsl::not_political.eq(dsl::not_political + self.not_political),
                dsl::impressions.eq(dsl::impressions + self.impressions),
                dsl::updated_at.eq(Utc::now()),
            ))
            .get_result(&*connection)?;

        // overwrite the old targeting/targets if the old one was empty.
        if self.targeting.is_some() && !saved_ad.targeting.is_some() {
            let new_targets = get_targets(&saved_ad.targeting.clone());
            diesel::update(ads::table.find(self.id))
                .set((
                    dsl::targeting.eq(&self.targeting),
                    dsl::targets.eq(new_targets.clone()),
                    dsl::targetedness.eq(get_targetedness_score(new_targets))
                ))
                .execute(&*connection)?;
        };

        if self.targeting.is_some() && saved_ad.created_at != saved_ad.updated_at {
            diesel::update(ads::table.find(self.id))
                .set((
                    dsl::targetings.eq(
                        self.targeting.clone().map_or(None, |targ| {
                            let mut targetings = saved_ad.targetings.clone().unwrap_or_else(|| vec![]);
                            targetings.push(targ);
                            Some(targetings)
                        })
                        
                        ),
                    dsl::targets.eq(
                        saved_ad.targets.clone().map(|old_targets_json| {
                            let mut old_targets_val = serde_json::to_value(old_targets_json).unwrap();
                            let old_targets = old_targets_val.as_array_mut().unwrap();
                            let new_targets : Vec<Value> = collect_targeting(&self.targeting.clone().unwrap()).unwrap_or(vec![])
                                .iter().map(|t| serde_json::to_value(t).unwrap()).collect();
                            let mut all_targets = old_targets.clone();
                            all_targets.extend(&mut new_targets.iter().cloned());
                            all_targets.sort_unstable_by(|x, y| x.to_string().cmp(&y.to_string()) );
                            all_targets.dedup();
                            serde_json::to_value(all_targets).unwrap()
                        })
                    ),
                ))
                .execute(&*connection)?;
        };
        

        Ok(saved_ad)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ad_parsing() {
        let ad = include_str!("./html-test.txt");
        let post = AdPost {
            id: "test".to_string(),
            html: ad.to_string(),
            political: None,
            targeting: None
        };
        let new_ad = NewAd::new(&post, "en-US").unwrap();
        assert!(new_ad.thumbnail.len() > 0);
        assert_eq!(new_ad.images.len(), 2);
        assert!(new_ad.title.len() > 0);

        assert_eq!(
            kuchiki::parse_html().one(new_ad.message).text_contents(),
            kuchiki::parse_html().one("<p><a class=\"_58cn\" href=\"https://www.facebook.com/hashtag/valerian\"><span class=\"_5afx\"><span class=\"_58cl _5afz\">#</span><span class=\"_58cm\">Valerian</span></span></a> is “the best experience since ‘Avatar.’” See it in 3D and RealD3D theaters this Friday. Get tickets now: <a>ValerianTickets.com</a></p>").text_contents()
        );
    }

    #[test]
    fn page_href_parsing() {
        let ad = include_str!("./another-html-test.txt");
        let post = AdPost {
            id: "test".to_string(),
            html: ad.to_string(),
            political: None,
            targeting: None
        };
        let new_ad = NewAd::new(&post, "en-US").unwrap();
        assert!(new_ad.thumbnail.len() > 0);
        assert!(new_ad.title.len() > 0);
        println!("{:?}", new_ad.page);
        assert!(new_ad.page.unwrap_or_else(|| String::from("")).len() > 0);
    }

    #[test]
    fn image_parsing() {
        use chrono::prelude::*;
        let ad = include_str!("./html-test.txt");
        let document = kuchiki::parse_html().one(ad);

        let saved_ad = Ad {
            id: "test".to_string(),
            html: ad.to_string(),
            political: 1,
            not_political: 2,
            title: get_title(&document).unwrap(),
            message: get_message(&document).unwrap(),
            thumbnail: get_image(&document).unwrap(),
            created_at: Utc::now(),
            updated_at: Utc::now(),
            lang: "US".to_string(),
            images: get_images(&document).unwrap(),
            impressions: 1,
            targeting: None,
            political_probability: 0.0,
            suppressed: false,
            targets: None,
            advertiser: None,
            entities: None,
            page: None,
            lower_page: None,
            targetings: Some(vec![]),
            paid_for_by: None,
            targetedness: Some(0)
        };
        let urls = saved_ad.image_urls();
        let images = Images::from_ad(&saved_ad, &urls).unwrap();
        assert!(images.html != saved_ad.html);
        assert!(!images.html.contains("fbcdn"));
        assert!(!images.html.contains("html"));
        assert_eq!(images.images.len(), saved_ad.images.len());
        assert!(images.thumbnail.unwrap() != saved_ad.thumbnail);
    }
}
