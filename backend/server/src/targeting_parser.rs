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
}

named!(until_b(&str) -> &str, take_until!("</b>"));

named!(provider(&str) -> Targeting,
    do_parse!(
        take_until_and_consume!("wants to reach people who are part of an audience") >>
        take_until_and_consume!("<b>") >>
        agency: until_b >> (Targeting::Agency(agency))
    )
);

named!(advertiser(&str) -> Targeting,
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

named!(segment(&str) -> Targeting,
    do_parse!(
        tag!("\"") >>
        segment: take_until!("\"") >>
        tag!("\"") >>
        (Targeting::Segment(segment))
    )
);

named!(gender(&str) -> Option<Targeting>,
    do_parse!(
        take_until_and_consume!("<b>") >>
        result: alt!(
            tag!("men") => {|_| Some(Targeting::Gender("Men")) } |
            tag!("women") => {|_| Some(Targeting::Gender("Women")) } |
            tag!("people") => {|_| None }
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
        res: tag!("people who may be similar to their customers") >>
        (Targeting::Retargeting(res))
    )
);

named!(age(&str) -> Option<Targeting>,
    do_parse!(
        ws!(tag!("age")) >>
        ws!(opt!(tag!("s"))) >>
        ws!(opt!(tag!("d"))) >>
        complete: ws!(take_until_and_consume!(" who")) >>
        (Some(Targeting::Age(complete)))
    )
);

named!(country(&str) -> Vec<Option<Targeting>>,
    do_parse!(country: until_b >> (vec![Some(Targeting::Region(country))]))
);

named!(city_state(&str) -> Vec<Option<Targeting>>,
    do_parse!(
        ws!(alt!(take_until_and_consume!("near") | take_until_and_consume!("in"))) >>
        city: ws!(take_until_and_consume!(",")) >>
        state: until_b >>
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
    do_parse!(
        advertiser: dbg!(advertiser) >>
        sources_and_interests: dbg!(alt!(interest | retargeting | website | provider)) >>
        gender: dbg!(gender) >>
        location: dbg!(age_and_location) >>
        (vec![&vec![Some(advertiser), Some(sources_and_interests), gender][..], &location[..]]
              .concat().iter().filter_map(|x| x.clone()).collect())
    )
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
            segment("\"Generation X\""),
            IResult::Done("", Targeting::Segment("Generation X"))
        );
        let g = "Google";
        assert_eq!(
            advertiser(&format!("<b id=\"ad_prefs_advertiser\">{}</b>", g)),
            IResult::Done("</b>", Targeting::Advertiser(g))
        );
        let i = "American Federation of State, County and Municipal Employees";
        assert_eq!(
            interest(&format!("<b id=\"ad_prefs_interest\">{}</b>", i)),
            IResult::Done("</b>", Targeting::Interest(i))
        );
        assert_eq!(
            gender("<b>men"),
            IResult::Done("", Some(Targeting::Gender("Men")))
        );
        assert_eq!(
            gender("<b>women"),
            IResult::Done("", Some(Targeting::Gender("Women")))
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
            .limit(10)
            .load::<Ad>(&connection)
            .unwrap();

        for ad in adverts {
            let t = ad.clone().targeting.unwrap();
            let targets = parse_targeting(&t);

            if targets.is_err() {
                println!("{:?}", targets);
                println!("{:?}", ad.clone().targeting);
                assert!(false)
            }
        }
    }
}
