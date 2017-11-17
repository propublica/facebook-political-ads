use errors::*;
use nom::IResult;


#[derive(Clone, Debug, PartialEq)]
pub enum Targeting<'a> {
    Gender(&'a str),
    City(&'a str),
    State(&'a str),
    Region(&'a str),
    Age(&'a str),
    Interest(&'a str),
    Segment(&'a str),
    Advertiser(&'a str),
    Retargeting(&'a str),
    Agency(&'a str),
    Website(&'a str),
    Language(&'a str),
    Like,
    List,
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

named!(provider(&str) -> Targeting,
    do_parse!(
        take_until_and_consume!("based on data provided by") >>
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
           take_until_and_consume!("who speak <b>\"") >>
           language: take_until_and_consume!("\"") >>
           until_b >>
           (Targeting::Language(language))
       )
   )
);

named!(segment(&str) -> Targeting,
    do_parse!(
        take_until_and_consume!("<b>") >>
        tag!("\"") >>
        segment: take_until_and_consume!("\"") >>
        (Targeting::Segment(segment))
    )
);

named!(gender(&str) -> Option<Targeting>,
    do_parse!(
        take_until_and_consume!("<b>") >>
        result: alt!(
            alt!(tag!("men") | tag!("Männer")) => {|m| Some(Targeting::Gender(m)) }
            | alt!(tag!("women") | tag!("Frauen") | tag!("kvinder")) => {|f| Some(Targeting::Gender(f)) }
            | alt!(tag!("people") | tag!("Personen") | tag!("personer")) => {|_| None }
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
        ) >>
        (Targeting::Retargeting(res))
    )
);

named!(age(&str) -> Option<Targeting>,
    alt!(
        do_parse!(
          ws!(alt!(tag!("zwischen") | tag!("im Alter von") | tag!("i alderen") | tag!("på"))) >>
          complete: ws!(take_until_and_consume!(",")) >>
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
        a: ws!(age) >>
        l: ws!(alt!(city_state | country)) >>
        (vec![&vec![a][..], &l[..]].concat())
    )
);

named!(targeting(&str) -> Vec<Targeting>,
    dbg_dmp!(do_parse!(
        advertiser_first: opt!(advertiser_b) >>
        sources_and_interests: opt!(alt!(
            interest
            | retargeting
            | website
            | provider
            | language
            | segment
            | like
            | list
        )) >>
        advertiser_second: opt!(advertiser_wants) >>
        gender: gender >>
        location: age_and_location >>
        (vec![&vec![advertiser_first,
                    sources_and_interests,
                    advertiser_second,
                    gender][..], &location[..]]
              .concat().iter().filter_map(|x| x.clone()).collect())
    ))
);

fn parse_targeting(thing: &str) -> Result<Vec<Targeting>> {
    match targeting(thing) {
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
            segment("<b>\"Generation X\""),
            IResult::Done("", Targeting::Segment("Generation X"))
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
            .limit(1000)
            .load::<Ad>(&connection)
            .unwrap();

        for ad in adverts {
            let t = ad.clone().targeting.unwrap();
            let targets = parse_targeting(&t);
            println!("{:?}", ad.clone().targeting);
            println!("{:?}", targets);
            if targets.is_err() {
                assert!(false)
            }
        }
    }
}
