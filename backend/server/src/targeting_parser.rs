use errors::*;
use nom::IResult;
use schema::targeting_info;

#[derive(Insertable)]
#[table_name = "targeting_info"]
pub struct TargetingInfo<'a> {
    targeting: &'a str,
    segment: Option<&'a str>,
}

impl<'a> TargetingInfo<'a> {
    fn new(targeting: &'a str, segment: Option<&'a str>) -> Self {
        TargetingInfo {
            targeting: targeting,
            segment: segment,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Targeting<'a> {
    Gender(&'a str),
    City(&'a str),
    State(&'a str),
    Region(&'a str),
    Age(&'a str),
    Interest(&'a str),
    Segment(&'a str),
    Retargeting(&'a str),
    Agency(&'a str),
    Website(&'a str),
    Language(&'a str),
    Employer(&'a str),
    School(&'a str),
    Like,
    List,
    // This is a special field that allows us to tie a page to a name
    Advertiser(&'a str),
}

impl<'a> From<TargetingInfo<'a>> for Targeting<'a> {
    fn from(row: TargetingInfo<'a>) -> Self {
        match row.targeting {
            "Gender" => Targeting::Gender(row.segment.unwrap()),
            "City" => Targeting::City(row.segment.unwrap()),
            "State" => Targeting::State(row.segment.unwrap()),
            "Region" => Targeting::Region(row.segment.unwrap()),
            "Age" => Targeting::Age(row.segment.unwrap()),
            "Interest" => Targeting::Interest(row.segment.unwrap()),
            "Segment" => Targeting::Segment(row.segment.unwrap()),
            "Retargeting" => Targeting::Retargeting(row.segment.unwrap()),
            "Agency" => Targeting::Agency(row.segment.unwrap()),
            "Website" => Targeting::Website(row.segment.unwrap()),
            "Language" => Targeting::Language(row.segment.unwrap()),
            "Employer" => Targeting::Employer(row.segment.unwrap()),
            "School" => Targeting::School(row.segment.unwrap()),
            "Like" => Targeting::Like,
            "List" => Targeting::List,
            _ => panic!("Trying to convert an unknown enum"),
        }
    }
}

impl<'a> From<Targeting<'a>> for TargetingInfo<'a> {
    fn from(targeting: Targeting<'a>) -> Self {
        match targeting {
            Targeting::Gender(s) => TargetingInfo::new("Gender", Some(s)),
            Targeting::City(s) => TargetingInfo::new("City", Some(s)),
            Targeting::State(s) => TargetingInfo::new("State", Some(s)),
            Targeting::Region(s) => TargetingInfo::new("Region", Some(s)),
            Targeting::Age(s) => TargetingInfo::new("Age", Some(s)),
            Targeting::Interest(s) => TargetingInfo::new("Interest", Some(s)),
            Targeting::Segment(s) => TargetingInfo::new("Segment", Some(s)),
            Targeting::Retargeting(s) => TargetingInfo::new("Retargeting", Some(s)),
            Targeting::Agency(s) => TargetingInfo::new("Agency", Some(s)),
            Targeting::Website(s) => TargetingInfo::new("Website", Some(s)),
            Targeting::Language(s) => TargetingInfo::new("Language", Some(s)),
            Targeting::Employer(s) => TargetingInfo::new("Employer", Some(s)),
            Targeting::School(s) => TargetingInfo::new("School", Some(s)),
            Targeting::Like => TargetingInfo::new("Like", None),
            Targeting::List => TargetingInfo::new("List", None),
            // This is a special field that allows us to tie a page to a name, it is a bit
            // unfortunate that we include it here, but that's ok because it helps out with below.
            Targeting::Advertiser(_) => panic!("Advertiser is not allowed as a targeting field"),
        }
    }
}

named!(until_b(&str) -> &str, take_until!("</b>"));

named!(list(&str) -> Targeting,
   do_parse!(
       take_until!("added you to a list of people they want to reach on Facebook.") >>
       (Targeting::List)
   )
);

named!(like(&str) -> Targeting,
   do_parse!(
       alt!(
           take_until!("who like their")
           | take_until!("whose friends like their")
           | take_until!("Personen erreichen möchte, denen deren Seite gefällt.")
       ) >>
       (Targeting::Like)
   )
);

named!(school(&str) -> Targeting,
    do_parse!(
        take_until_and_consume!("som har angivet skolen <b>") >>
        res: take_until!("</b> på") >>
        (Targeting::Employer(res))
    )
);

named!(employer(&str) -> Targeting,
    do_parse!(
        take_until_and_consume!("Personen erreichen möchte, die <b>") >>
        res: take_until!("</b> als Arbeitgeber") >>
        (Targeting::Employer(res))
    )
);

named!(provider(&str) -> Targeting,
    do_parse!(
        alt!(
            take_until!("based on data provided by") |
            take_until!("wir basierend auf Daten von")
        ) >>
        take_until_and_consume!("<b>") >>
        agency: until_b >> (Targeting::Agency(agency))
    )
);

named!(advertiser_wants(&str) -> Targeting,
    do_parse!(
        take_until_and_consume!("including that ") >>
        advertiser: take_until!(" wants to reach") >>
        (Targeting::Advertiser(advertiser))
    )
);

named!(advertiser_b(&str) -> Targeting,
    do_parse!(
        take_until_and_consume!("<b id=\"ad_prefs_advertiser\">") >>
        advertiser: until_b >>
        (Targeting::Advertiser(advertiser))
    )
);

named!(interest(&str) -> Targeting,
   do_parse!(
        take_until_and_consume!("<b id=\"ad_prefs_interest\">") >>
        interest: until_b >>
        (Targeting::Interest(interest))
    )
);

named!(language(&str) -> Targeting,
   alt!(
       do_parse!(
           take_until_and_consume!("die <b>") >>
           language: take_until!("</b> sprechen") >>
           (Targeting::Language(language))
       )
       | do_parse!(
           take_until_and_consume!("who speak <b>") >>
           opt!(tag!("\"")) >>
           language: alt!(until_b | take_until_and_consume!("\"")) >>
           (Targeting::Language(language))
       )
   )
);

named!(segment(&str) -> Targeting,
    do_parse!(
        alt!(
            take_until_and_consume!("„<b>") |
            take_until_and_consume!("<b>\"") |
            take_until_and_consume!("<b>„")
        ) >>
        segment: alt!(
            take_until!("\"</b>") |
            take_until!("</b>“") |
            take_until!("“</b>")
        ) >>
        (Targeting::Segment(segment))
    )
);

named!(gender(&str) -> Option<Targeting>,
    do_parse!(
        take_until_and_consume!("<b>") >>
        result: alt!(
            alt!(
                tag!("men")
                | tag!("Männer")
                | tag!("mænd")
                | tag!("gli uomini")
                | tag!("män")
            ) => {|m| Some(Targeting::Gender(m)) }
            | alt!(
                tag!("women")
                | tag!("Frauen")
                | tag!("kvinder")
            ) => {|f| Some(Targeting::Gender(f)) }
            | alt!(
                tag!("people")
                | tag!("Personen")
                | tag!("personer")
                | tag!("le persone")
            ) => {|_| None }
        ) >>
        (result)
    )
);

named!(website(&str) -> Targeting,
   do_parse!(
       take_until_and_consume!("wants to reach ") >>
       res: tag!("people who have visited their website or used one of their apps") >>
       (Targeting::Website(res))
   )
);

named!(retargeting(&str) -> Targeting,
    do_parse!(
        take_until_and_consume!("<b>") >>
        res: alt!(
            tag!("people who may be similar to their customers")
            | tag!("Personen, die deren Kunden ähneln")
            | tag!("personer, som minder om deres kunder")
            | tag!("i nærheden af deres virksomhed for nylig")
            | tag!("kürzlich in der Nähe des Unternehmens")
            | tag!("nyss varit i närheten av företaget")
            | tag!("recently near their business")
        ) >>
        (Targeting::Retargeting(res))
    )
);

named!(age(&str) -> Option<Targeting>,
    alt!(
        do_parse!(
          ws!(alt!(
              tag!("zwischen")
              | tag!("im Alter von")
              | tag!("i alderen")
              | tag!("i åldern")
              | tag!("på")
              | tag!("di età")
          )) >>
          complete: ws!(alt!(
              take_until_and_consume!(",")
              | take_until_and_consume!("in")
              | take_until!("che vivono")
          )) >>
          (Some(Targeting::Age(complete)))
        ) |
        do_parse!(
            ws!(tag!("age")) >>
            ws!(opt!(tag!("s"))) >>
            ws!(opt!(tag!("d"))) >>
            complete: ws!(take_until_and_consume!(" who")) >>
            (Some(Targeting::Age(complete)))
        )
    )
);

named!(country(&str) -> Vec<Option<Targeting>>,
    do_parse!(country: until_b >> (vec![Some(Targeting::Region(country))]))
);

named!(city_state(&str) -> Vec<Option<Targeting>>,
    do_parse!(
        ws!(alt!(
            take_until_and_consume!("near") |
            take_until_and_consume!("in") |
            take_until_and_consume!("af")
        )) >>
        city: ws!(take_until_and_consume!(",")) >>
        state: alt!(take_until!("wohnen oder") | until_b) >>
        (vec![Some(Targeting::City(city)), Some(Targeting::State(state))])
    )
);

named!(age_and_location(&str) -> Vec<Option<Targeting>>,
    do_parse!(
        a: dbg!(ws!(age)) >>
        l: ws!(alt!(city_state | dbg!(country))) >>
        (vec![&vec![a][..], &l[..]].concat())
    )
);

named!(get_targeting(&str) -> Vec<Targeting>,
    do_parse!(
        advertiser_first: opt!(advertiser_b) >>
        sources_and_interests: opt!(alt!(
            interest
            | retargeting
            | employer
            | school
            | website
            | provider
            | language
            | segment
            | like
            | list
        )) >>
        advertiser_second: opt!(advertiser_wants) >>
        gender: dbg!(gender) >>
        location: dbg!(age_and_location) >>
        (vec![&vec![advertiser_first,
                    sources_and_interests,
                    advertiser_second,
                    gender][..], &location[..]]
              .concat().iter().filter_map(|x| x.clone()).collect())
    )
);

pub fn collect_targeting(thing: &str) -> Result<Vec<Targeting>> {
    match parse_targeting(thing) {
        Ok(result) => Ok(
            result
                .into_iter()
                .filter(|t| match t {
                    &Targeting::Advertiser(_) => false,
                    _ => true,
                })
                .collect(),
        ),
        e => e,
    }
}

pub fn collect_advertiser(thing: &str) -> Option<Targeting> {
    match parse_targeting(thing) {
        Ok(result) => {
            result
                .into_iter()
                .filter(|t| match t {
                    &Targeting::Advertiser(_) => true,
                    _ => false,
                })
                .nth(0)
        }
        _ => None,
    }
}

fn parse_targeting(thing: &str) -> Result<Vec<Targeting>> {
    match get_targeting(thing) {
        IResult::Done(_, result) => Ok(result),
        IResult::Error(e) => Err(e.into()),
        IResult::Incomplete(e) => Err(ErrorKind::TargetingIncomplete(e).into()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_targeting() {
        assert_eq!(
            segment("<b>\"Generation X\"</b>"),
            IResult::Done("\"</b>", Targeting::Segment("Generation X"))
        );
        let g = "Google";
        assert_eq!(
            advertiser_b(&format!("<b id=\"ad_prefs_advertiser\">{}</b>", g)),
            IResult::Done("</b>", Targeting::Advertiser(g))
        );
        let i = "American Federation of State, County and Municipal Employees";
        assert_eq!(
            interest(&format!("<b id=\"ad_prefs_interest\">{}</b>", i)),
            IResult::Done("</b>", Targeting::Interest(i))
        );
        assert_eq!(
            gender("<b>men"),
            IResult::Done("", Some(Targeting::Gender("men")))
        );
        assert_eq!(
            gender("<b>women"),
            IResult::Done("", Some(Targeting::Gender("women")))
        );
        assert_eq!(gender("<b>people"), IResult::Done("", None));
        assert_eq!(
            age(" ages 26 to 62 who "),
            IResult::Done("", Some(Targeting::Age("26 to 62")))
        );
        assert_eq!(
            country("United States</b>"),
            IResult::Done("</b>", vec![Some(Targeting::Region("United States"))])
        );
        assert_eq!(
            city_state("near Burlington, North Carolina</b>"),
            IResult::Done(
                "</b>",
                vec![
                    Some(Targeting::City("Burlington")),
                    Some(Targeting::State("North Carolina")),
                ],
            )
        );
        assert_eq!(
            age_and_location(
                "ages 18 and older who live or were recently near San Francisco, California</b>",
            ),
            IResult::Done(
                "</b>",
                vec![
                    Some(Targeting::Age("18 and older")),
                    Some(Targeting::City("San Francisco")),
                    Some(Targeting::State("California")),
                ],
            )
        );
    }

    #[test]
    fn test_all() {
        use dotenv::dotenv;
        use diesel::prelude::*;
        use diesel::pg::PgConnection;
        use models::Ad;
        use schema::ads::dsl::*;
        use std::env;
        dotenv().ok();
        let database_url = env::var("DATABASE_URL").unwrap();
        let connection = PgConnection::establish(&database_url).unwrap();
        let adverts = ads.filter(targeting.is_not_null())
            .limit(100000)
            .load::<Ad>(&connection)
            .unwrap();

        for ad in adverts {
            let t = ad.clone().targeting.unwrap();
            let targets = parse_targeting(&t);
            if targets.is_err() {
                println!("{:?}", ad.clone().targeting);

                assert!(false);
            }
        }
    }
}
