use errors::*;
use nom::IResult;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Targeting {
    target: String,
    #[serde(skip_serializing_if = "Option::is_none")] pub segment: Option<String>,
}

impl Targeting {
    fn new(targeting: &str, segment: Option<&str>) -> Self {
        Targeting {
            target: targeting.to_string(),
            segment: match segment {
                Some(s) => Some(s.to_string()),
                None => None,
            },
        }
    }
}

impl<'a> From<TargetingParsed<'a>> for Targeting {
    fn from(targeting: TargetingParsed<'a>) -> Self {
        match targeting {
            TargetingParsed::Gender(s) => Targeting::new("Gender", Some(s)),
            TargetingParsed::City(s) => Targeting::new("City", Some(s)),
            TargetingParsed::State(s) => Targeting::new("State", Some(s)),
            TargetingParsed::Region(s) => Targeting::new("Region", Some(s)),
            TargetingParsed::Age(s) => Targeting::new("Age", Some(s)),
            TargetingParsed::Interest(s) => Targeting::new("Interest", Some(s)),
            TargetingParsed::Segment(s) => Targeting::new("Segment", Some(s)),
            TargetingParsed::Retargeting(s) => Targeting::new("Retargeting", Some(s)),
            TargetingParsed::Agency(s) => Targeting::new("Agency", Some(s)),
            TargetingParsed::Website(s) => Targeting::new("Website", Some(s)),
            TargetingParsed::Language(s) => Targeting::new("Language", Some(s)),
            TargetingParsed::Employer(s) => Targeting::new("Employer", Some(s)),
            TargetingParsed::School(s) => Targeting::new("School", Some(s)),
            TargetingParsed::Like => Targeting::new("Like", None),
            TargetingParsed::List => Targeting::new("List", None),
            TargetingParsed::Advertiser(_) => {
                panic!("Advertiser is not allowed as a targeting field")
            }
        }
    }
}

// https://github.com/Geal/nom/issues/613
trait DbgFakeHex {
    fn to_hex(&self, chunk_size: usize) -> String;
}

impl DbgFakeHex for str {
    fn to_hex(&self, chunk_size: usize) -> String {
        use nom::HexDisplay;
        self.as_bytes().to_hex(chunk_size)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TargetingParsed<'a> {
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

named!(until_b(&str) -> &str, take_until!("</b>"));

named!(list(&str) -> TargetingParsed,
   do_parse!(
       take_until!("added you to a list of people they want to reach on Facebook.") >>
       (TargetingParsed::List)
   )
);

named!(like(&str) -> TargetingParsed,
   do_parse!(
       alt!(
           take_until!("who like their")
           | take_until!("whose friends like their")
           | take_until!("Personen erreichen möchte, denen deren Seite gefällt.")
       ) >>
       (TargetingParsed::Like)
   )
);

named!(school(&str) -> TargetingParsed,
    do_parse!(
        take_until_and_consume!("som har angivet skolen <b>") >>
        res: take_until!("</b> på") >>
        (TargetingParsed::Employer(res))
    )
);

named!(employer(&str) -> TargetingParsed,
    do_parse!(
        take_until_and_consume!("Personen erreichen möchte, die <b>") >>
        res: take_until!("</b> als Arbeitgeber") >>
        (TargetingParsed::Employer(res))
    )
);

named!(provider(&str) -> TargetingParsed,
    do_parse!(
        alt!(
            take_until!("based on data provided by") |
            take_until!("wir basierend auf Daten von")
        ) >>
        take_until_and_consume!("<b>") >>
        agency: until_b >> (TargetingParsed::Agency(agency))
    )
);

named!(advertiser_wants(&str) -> TargetingParsed,
    do_parse!(
        take_until_and_consume!("including that ") >>
        advertiser: take_until!(" wants to reach") >>
        (TargetingParsed::Advertiser(advertiser))
    )
);

named!(advertiser_b(&str) -> TargetingParsed,
    do_parse!(
        alt!(
            take_until_and_consume!("is that <b>") |
            take_until_and_consume!("<b id=\"ad_prefs_advertiser\">")
        ) >>
        advertiser: until_b >>
        (TargetingParsed::Advertiser(advertiser))
    )
);

named!(interest(&str) -> TargetingParsed,
   do_parse!(
        take_until_and_consume!("<b id=\"ad_prefs_interest\">") >>
        interest: until_b >>
        (TargetingParsed::Interest(interest))
    )
);

named!(language(&str) -> TargetingParsed,
   alt!(
       do_parse!(
           take_until_and_consume!("die <b>") >>
           language: take_until!("</b> sprechen") >>
           (TargetingParsed::Language(language))
       )
       | do_parse!(
           take_until_and_consume!("who speak <b>") >>
           opt!(tag!("\"")) >>
           language: alt!(take_until_and_consume!("\"</b>") | until_b) >>
           (TargetingParsed::Language(language))
       )
   )
);

named!(segment(&str) -> TargetingParsed,
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
        (TargetingParsed::Segment(segment))
    )
);

named!(gender(&str) -> Option<TargetingParsed>,
    do_parse!(
        take_until_and_consume!("<b>") >>
        result: alt!(
            alt!(
                tag!("men")
                | tag!("Männer")
                | tag!("mænd")
                | tag!("gli uomini")
                | tag!("män")
            ) => {|m| Some(TargetingParsed::Gender(m)) }
            | alt!(
                tag!("women")
                | tag!("Frauen")
                | tag!("kvinder")
            ) => {|f| Some(TargetingParsed::Gender(f)) }
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

named!(website(&str) -> TargetingParsed,
   do_parse!(
       take_until_and_consume!("wants to reach ") >>
       res: tag!("people who have visited their website or used one of their apps") >>
       (TargetingParsed::Website(res))
   )
);

named!(retargeting(&str) -> TargetingParsed,
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
        (TargetingParsed::Retargeting(res))
    )
);

named!(age(&str) -> Option<TargetingParsed>,
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
              take_until_and_consume!("derover")
              | take_until_and_consume!("älter")
              | take_until_and_consume!("die in")
              | take_until_and_consume!("che vivono")
              | take_until_and_consume!(",")
          )) >>
          (Some(TargetingParsed::Age(complete)))
        ) |
        do_parse!(
            ws!(tag!("age")) >>
            ws!(opt!(tag!("s"))) >>
            ws!(opt!(tag!("d"))) >>
            complete: ws!(take_until_and_consume!(" who")) >>
            (Some(TargetingParsed::Age(complete)))
        )
    )
);

named!(region(&str) -> Vec<Option<TargetingParsed>>,
    do_parse!(
         tag!("live") >>
         ws!(take_until_and_consume!("in")) >>
         country: until_b >>
         (vec![Some(TargetingParsed::Region(country))])
     )
);

named!(city_state(&str) -> Vec<Option<TargetingParsed>>,
    do_parse!(
        ws!(alt!(
            take_until_and_consume!("near") |
            take_until_and_consume!("in") |
            take_until_and_consume!("af")
        )) >>
        city: ws!(take_until_and_consume!(",")) >>
        state: alt!(take_until!("wohnen oder") | until_b) >>
        (vec![Some(TargetingParsed::City(city)), Some(TargetingParsed::State(state))])
    )
);

named!(age_and_location(&str) -> Vec<Option<TargetingParsed>>,
    do_parse!(
        a: ws!(age) >>
        l: ws!(alt!(city_state | region)) >>
        (vec![&vec![a][..], &l[..]].concat())
    )
);

named!(get_targeting(&str) -> Vec<TargetingParsed>,
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
        gender: gender >>
        location: age_and_location >>
        (vec![&vec![advertiser_first,
                    sources_and_interests,
                    advertiser_second,
                    gender][..], &location[..]]
              .concat().iter().filter_map(|x| x.clone()).collect())
    )
);

pub fn collect_targeting(thing: &str) -> Result<Vec<Targeting>> {
    parse_targeting(thing).map(|result| {
        result
            .into_iter()
            .filter(|t| match *t {
                TargetingParsed::Advertiser(_) => false,
                _ => true,
            })
            .map(Targeting::from)
            .collect()
    })
}

pub fn collect_advertiser(thing: &str) -> Option<String> {
    match parse_targeting(thing) {
        Ok(result) => result
            .into_iter()
            .filter(|t| match *t {
                TargetingParsed::Advertiser(_) => true,
                _ => false,
            })
            .nth(0)
            .map(|t| match t {
                TargetingParsed::Advertiser(a) => a.to_string(),
                _ => panic!("Somehow got something other than an Advertiser"),
            }),
        _ => None,
    }
}

pub fn parse_targeting(thing: &str) -> Result<Vec<TargetingParsed>> {
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
            IResult::Done("\"</b>", TargetingParsed::Segment("Generation X"))
        );
        let g = "Google";
        assert_eq!(
            advertiser_b(&format!("<b id=\"ad_prefs_advertiser\">{}</b>", g)),
            IResult::Done("</b>", TargetingParsed::Advertiser(g))
        );
        let i = "American Federation of State, County and Municipal Employees";
        assert_eq!(
            interest(&format!("<b id=\"ad_prefs_interest\">{}</b>", i)),
            IResult::Done("</b>", TargetingParsed::Interest(i))
        );
        assert_eq!(
            gender("<b>men"),
            IResult::Done("", Some(TargetingParsed::Gender("men")))
        );
        assert_eq!(
            gender("<b>women"),
            IResult::Done("", Some(TargetingParsed::Gender("women")))
        );
        assert_eq!(gender("<b>people"), IResult::Done("", None));
        assert_eq!(
            age(" ages 26 to 62 who "),
            IResult::Done("", Some(TargetingParsed::Age("26 to 62")))
        );
        assert_eq!(
            region("live in United States</b>"),
            IResult::Done("</b>", vec![Some(TargetingParsed::Region("United States"))])
        );
        assert_eq!(
            city_state("near Burlington, North Carolina</b>"),
            IResult::Done(
                "</b>",
                vec![
                    Some(TargetingParsed::City("Burlington")),
                    Some(TargetingParsed::State("North Carolina")),
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
                    Some(TargetingParsed::Age("18 and older")),
                    Some(TargetingParsed::City("San Francisco")),
                    Some(TargetingParsed::State("California")),
                ],
            )
        );
    }
}
