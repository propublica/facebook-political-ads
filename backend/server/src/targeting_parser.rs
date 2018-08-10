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
            TargetingParsed::MinAge(s) => Targeting::new("MinAge", Some(s)),
            TargetingParsed::MaxAge(s) => Targeting::new("MaxAge", Some(s)),
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
            TargetingParsed::EngagedWithContent => Targeting::new("Engaged with Content", None),
            TargetingParsed::ActivityOnTheFacebookFamily => Targeting::new("Activity on the Facebook Family", None),
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
    MinAge(&'a str),
    MaxAge(&'a str),
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
    EngagedWithContent,
    ActivityOnTheFacebookFamily,
    // This is a special field that allows us to tie a page to a name
    Advertiser(&'a str),
}

named!(until_b(&str) -> &str, take_until!("</b>"));

// variations.
// you're in a customer list
// you're on a customer list
// your email address and phone number are on a customer list
// your email address is on a customer list
// your phone number and email address are on a customer list
// your phone number is in a customer list
// your phone number is on a customer list
named!(list(&str) -> TargetingParsed,
   do_parse!(
       take_until!("added you to a list of people they want to reach on Facebook.") >>
       (TargetingParsed::List)
   )
);

// two variants: `wants to reach people who have engaged with them or with their content`
//               `wants to reach people who engaged with them or their content`

named!(engaged_with_content(&str) -> TargetingParsed,
   do_parse!(
       alt!(
           take_until!("wants to reach people who engaged with them or their content.")
            | take_until!("wants to reach people who have engaged with them or with their content" )
        ) >>
       (TargetingParsed::EngagedWithContent)
   )
);
named!(activity_on_the_facebook_family(&str) -> TargetingParsed, 
    do_parse!(
        take_until!("wants to reach people based on their activity on the Facebook family of apps and services") >>
        (TargetingParsed::ActivityOnTheFacebookFamily)
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
            take_until_and_consume!("is because <b>") |
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
    alt!(
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
        | do_parse!(
            take_until_and_consume!("an audience called '")  >>
            segment: take_until_and_consume!("'") >>
            (TargetingParsed::Segment(segment))
        )
        | do_parse!(
            take_until_and_consume!("an audience called \"")  >>
            segment: take_until_and_consume!("\"") >>
            (TargetingParsed::Segment(segment))
        )

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

named!(age(&str) -> TargetingParsed,
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
          (TargetingParsed::Age(complete))
        ) |
        do_parse!(
            ws!(tag!("age")) >>
            ws!(opt!(tag!("s"))) >>
            ws!(opt!(tag!("d"))) >>
            complete: alt!(ws!(take_until_and_consume!(" who")) | until_b ) >>
            (TargetingParsed::Age(complete))
        )
    )
);

#[derive(Clone, Debug, PartialEq)]
enum InternalMaxAge<'a> {
    Some(&'a str),
    None
}

named!(max_age_parser(&str) -> Option<InternalMaxAge>,
    do_parse!(
        max_age: alt_complete!(
            do_parse!(
                ws!(take_until_and_consume!("and ")) >>
                older: tag!("older") >>
                (older)
            ) | 
            do_parse!(
                ws!(take_until_and_consume!("to ")) >>
                actual_age: ws!(take_while1!(|n: char| n.is_numeric() )) >>
                (actual_age)
            ) | 
            do_parse!(
                tag!("") >>
                ("")
            )
        ) >>
        (match max_age {
            "" => None,
            "older" => Some(InternalMaxAge::None),
            _ => Some(InternalMaxAge::Some(max_age))
        })
    )
);

named!(min_max_age(&str) -> Vec<Option<TargetingParsed>>,
    do_parse!(
        min_age: complete!(opt!(ws!(take_while1!(|n: char| n.is_numeric() )))) >>
        max_age: max_age_parser >>
        (vec![
            min_age.map(|l| TargetingParsed::MinAge(l)), 
            max_age.or_else(|| // if there's no "and older" or "to NN", then it's just a single age -- or no age statement at all.
                match min_age {
                    Some(_) => Option::Some(InternalMaxAge::Some(min_age.unwrap())),
                    _ => None 
                }
            ).and_then(|max_age_|
                match max_age_ {
                    InternalMaxAge::Some(max_age_str) => Option::Some(TargetingParsed::MaxAge(max_age_str)), // if there's a max age, leave it be.
                    InternalMaxAge::None  => Option::None,       // if it's Some(None), then it's "and older", so  just return a None.
                }
            )
        ])
    )
);

// Donald J. Trump wants to reach <b>people who live in the United States</b>
named!(region(&str) -> Vec<Option<TargetingParsed>>,
    do_parse!(
         ws!(opt!(tag!("who"))) >>
         alt!(tag!("live") | tag!("were recently")) >>
         ws!(take_until_and_consume!("in")) >>
         country: until_b >>
         (vec![Some(TargetingParsed::Region(country))])
     )
);

named!(city_state(&str) -> Vec<Option<TargetingParsed>>,
    do_parse!(
        ws!(alt!(
            take_until_and_consume!("near ") |
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
        age: opt!(ws!(age)) >>
        location: ws!(alt!(city_state | region | value!( vec![] ) )) >>
        (vec![&vec![age.clone()][..], &min_max_age(match age.unwrap_or(TargetingParsed::Age("")) {
            TargetingParsed::Age(a) => a,
            _ => ""
        }).to_result().unwrap_or(vec![]).into_iter().filter(|ref i|i.is_some() ).collect::<Vec<Option<TargetingParsed>>>()[..], &location[..]].concat())
        
            
    )
);

// TODO: implement engaged_with_content
named!(get_targeting(&str) -> Vec<TargetingParsed>,
    alt!(
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
                        | activity_on_the_facebook_family
            )) >>
            advertiser_second: opt!(advertiser_wants) >>
            gender: gender >>
            location: age_and_location >>
            (vec![&vec![advertiser_first,
                        sources_and_interests,
                        advertiser_second,
                        gender][..], &location[..]]
                .concat().iter().filter_map(|x| x.clone()).collect())
        ) | 
        do_parse!(
            advertiser_first: opt!(advertiser_b) >>
            sources_and_interests: opt!(engaged_with_content) >> 
            (vec![advertiser_first, sources_and_interests].iter().filter_map(|x| x.clone()).collect())
        )
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
    use nom::ErrorKind::Complete;
    #[test]
    fn test_targeting() {



        assert_eq!(
            max_age_parser("and older"), 
            IResult::Done("", Some(InternalMaxAge::None))
        );
        assert_eq!(
            max_age_parser("to 64"), 
            IResult::Done("", Some(InternalMaxAge::Some("64")))
        );
        assert_eq!(
            max_age_parser(" and older"), 
            IResult::Done("", Some(InternalMaxAge::None))
        );
        assert_eq!(
            max_age_parser(" to 64"), 
            IResult::Done("", Some(InternalMaxAge::Some("64")))
        );


        assert_eq!(
            min_max_age("45 and older"), 
            IResult::Done("", vec![Some(TargetingParsed::MinAge("45")), None])
        );
        assert_eq!(
            min_max_age("45 to 64"), 
            IResult::Done("", vec![Some(TargetingParsed::MinAge("45")), Some(TargetingParsed::MaxAge("64"))])
        );
        assert_eq!(
            min_max_age(""), 
            IResult::Error(Complete)
        );
        assert_eq!(
            min_max_age("65 "), 
            IResult::Done("", vec![Some(TargetingParsed::MinAge("65")), Some(TargetingParsed::MaxAge("65"))])
        );


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
            IResult::Done("", TargetingParsed::Age("26 to 62"))
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
            segment(" an audience called 'Member of a family-based household.'"),
            IResult::Done(
                "",
                TargetingParsed::Segment("Member of a family-based household.")
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
                    Some(TargetingParsed::MinAge("18")),
                    Some(TargetingParsed::City("San Francisco")),
                    Some(TargetingParsed::State("California")),
                ],
            )
        );
        assert_eq!(
            age_and_location("ages 18 and older who live or were recently in Utah</b>",),
            IResult::Done(
                "</b>",
                vec![
                    Some(TargetingParsed::Age("18 and older")),
                    Some(TargetingParsed::MinAge("18")),
                    Some(TargetingParsed::Region("Utah")),
                ],
            )
        );
        assert_eq!(
            age_and_location(
                "aged 18 and older who live or have recently been in the United States</b>",
            ),
            IResult::Done(
                "</b>",
                vec![
                    Some(TargetingParsed::Age("18 and older")),
                    Some(TargetingParsed::MinAge("18")),
                    Some(TargetingParsed::Region("the United States")),
                ],
            )
        );
        assert_eq!(
            age_and_location("ages 25 and older who were recently in the United States</b>",),
            IResult::Done(
                "</b>",
                vec![
                    Some(TargetingParsed::Age("25 and older")),
                    Some(TargetingParsed::MinAge("25")),
                    Some(TargetingParsed::Region("the United States")),
                ],
            )
        );

        // One reason you're seeing this ad is that <b id="ad_prefs_advertiser">American Immigration Lawyers Association</b> wants to reach people interested in <b id="ad_prefs_interest">Immigration</b>, based on activity such as liking Pages or clicking on ads.</div></span><div class="_4hcd"><span>There may be other reasons you're seeing this ad, including that American Immigration Lawyers Association wants to reach <b>people ages 23 to 60 who live or were recently in the United States</b>.
        assert_eq!(
            advertiser_b(
                "<b id=\"ad_prefs_advertiser\">American Immigration Lawyers Association</b>"
            ),
            IResult::Done(
                "</b>",
                TargetingParsed::Advertiser("American Immigration Lawyers Association")
            )
        );
        assert_eq!(
            interest("<b id=\"ad_prefs_interest\">Immigration</b>"),
            IResult::Done("</b>", TargetingParsed::Interest("Immigration"))
        );
        assert_eq!(
            age_and_location("ages 23 to 60 who live or were recently in the United States</b>",),
            IResult::Done(
                "</b>",
                vec![
                    Some(TargetingParsed::Age("23 to 60")),
                    Some(TargetingParsed::MinAge("23")),
                    Some(TargetingParsed::MaxAge("60")),
                    Some(TargetingParsed::Region("the United States")),
                ]
            )
        );
        assert_eq!(
            advertiser_wants(
                "including that American Immigration Lawyers Association wants to reach",
            ),
            IResult::Done(
                " wants to reach",
                TargetingParsed::Advertiser("American Immigration Lawyers Association")
            )
        );

        //  <div><div class="_4-i0 _26c5"><div class="clearfix"><div class="_51-u rfloat _ohf"><a class="_42ft _5upp _50zy layerCancel _51-t _50-0 _50z-" href="nullblank">Close</a></div><div><h3 id="u_re_0" class="_52c9">About This Facebook Ad</h3></div></div></div><div class="_4-i2 _pig _4s3a _50f4"><div class="_4uov"><div class="_4uoz"><div id="u_re_1"></div><div class="_3-8x"><span class="_4v6n"><div id="u_re_2">One reason you're seeing this ad is that <b id="ad_prefs_advertiser">World Wildlife Fund</b> wants to reach people who have visited their website or used one of their apps. This is based on customer information provided by World Wildlife Fund.</div></span><div class="_4hcd"><span>There may be other reasons you're seeing this ad, including that World Wildlife Fund wants to reach <b>people ages 25 and older who live or were recently in the United States</b>. This is information based on your Facebook profile and where you've connected to the internet.</span>
        assert_eq!(
            advertiser_b("<b id=\"ad_prefs_advertiser\">World Wildlife Fund</b>"),
            IResult::Done("</b>", TargetingParsed::Advertiser("World Wildlife Fund"))
        );
        assert_eq!(
            website(
                "wants to reach people who have visited their website or used one of their apps"
            ),
            IResult::Done(
                "",
                TargetingParsed::Website(
                    "people who have visited their website or used one of their apps"
                )
            )
        );

    assert_eq!(get_targeting("<div><div class=\"_4-i0 _26c5\"><div class=\"clearfix\"><div class=\"_51-u rfloat _ohf\"><a class=\"_42ft _5upp _50zy layerCancel _51-t _50-0 _50z-\" href=\"nullblank\">Close</a></div><div><h3 id=\"u_19_0\" class=\"_52c9\">About this Facebook ad</h3></div></div></div><div class=\"_4-i2 _pig _4s3a _50f4\"><div class=\"_4uov\"><div class=\"_4uoz\"><div id=\"u_19_1\"></div><div class=\"_3-8x\"><span class=\"_4v6n\"><div id=\"u_19_2\">One reason why you\'re seeing this ad is that <b id=\"ad_prefs_advertiser\">Abdul El-Sayed</b> wants to reach people based on their activity on the Facebook family of apps and services. This includes sharing links to their website, interacting with their content (such as clicking ads, watching videos or saving content) or directly interacting (such as messaging) with them.</div></span><div class=\"_4hcd\"><span>There may be other reasons why you\'re seeing this ad, including that Abdul El-Sayed wants to reach <b>people aged 18 and older who live or have recently been in the United States</b>. This is information based on your Facebook profile and where you\'ve connected to the Internet.</span></div></div></div><div></div><div class=\"_4uor _52jw\"><div class=\"_5aj7\" id=\"u_19_3\"><div class=\"_4b").to_result(), Ok(vec![TargetingParsed::Advertiser("Abdul El-Sayed"), TargetingParsed::ActivityOnTheFacebookFamily, TargetingParsed::Advertiser("Abdul El-Sayed"), TargetingParsed::Age("18 and older"), TargetingParsed::MinAge("18"), TargetingParsed::Region("the United States")]));


    assert_eq!(get_targeting("<div><div class=\"_4-i0 _26c5\"><div class=\"clearfix\"><div class=\"_51-u rfloat _ohf\"><a class=\"_42ft _5upp _50zy layerCancel _51-t _50-0 _50z-\" href=\"nullblank\">Close</a></div><div><h3 id=\"u_56_0\" class=\"_52c9\">About This Facebook Ad</h3></div></div></div><div class=\"_4-i2 _pig _4s3a _50f4\"><div class=\"_4uov\"><div class=\"_4uoz\"><div id=\"u_56_1\"></div><div class=\"_3-8x\"><span class=\"_4v6n\"><div id=\"u_56_2\">One reason you\'re seeing this ad is that <b id=\"ad_prefs_advertiser\">Carri Hicks</b> wants to reach people who like their page.</div></span><div class=\"_4hcd\"><span>There may be other reasons you\'re seeing this ad, including that Carri Hicks wants to reach <b>people ages 18 and older who live in Oklahoma</b>. This is information based on your Facebook profile and where you\'ve connected to the internet.</span></div>").to_result(), Ok(vec![TargetingParsed::Advertiser("Carri Hicks"),TargetingParsed::Like, TargetingParsed::Advertiser("Carri Hicks"), TargetingParsed::Age("18 and older"), TargetingParsed::MinAge("18"), TargetingParsed::Region("Oklahoma")]));


        // assert_eq!(advertiser_b("<div><div class=\"_4-i0 _26c5\"><div class=\"clearfix\"><div class=\"_51-u rfloat _ohf\"><a class=\"_42ft _5upp _50zy layerCancel _51-t _50-0 _50z-\" href=\"nullblank\">Close</a></div><div><h3 id=\"u_19_0\" class=\"_52c9\">About this Facebook ad</h3></div></div></div><div class=\"_4-i2 _pig _4s3a _50f4\"><div class=\"_4uov\"><div class=\"_4uoz\"><div id=\"u_19_1\"></div><div class=\"_3-8x\"><span class=\"_4v6n\"><div id=\"u_19_2\">One reason why you\'re seeing this ad is that <b id=\"ad_prefs_advertiser\">Abdul El-Sayed</b> wants to reach people based on their activity on the Facebook family of apps and services. This includes sharing links to their website, interacting with their content (such as clicking ads, watching videos or saving content) or directly interacting (such as messaging) with them.</div></span><div class=\"_4hcd\"><span>There may be other reasons why you\'re seeing this ad, including that Abdul El-Sayed wants to reach <b>people aged 18 and older who live or have recently been in the United States</b>. This is information based on your Facebook profile and where you\'ve connected to the Internet.</span></div></div></div><div></div><div class=\"_4uor _52jw\"><div class=\"_5aj7\" id=\"u_19_3\"><div class=\"_4b").to_result(), Ok(TargetingParsed::Advertiser("Abdul El-Sayed")));

        // assert_eq!(activity_on_the_facebook_family("wants to reach people based on their activity on the Facebook family of apps and services. This includes sharing links to their website, interacting with their content (such as clicking ads, watching videos or saving content) or directly interacting (such as messaging) with them.</div></span><div class=\"_4hcd\"><span>There may be other reasons why you\'re seeing this ad, including that Abdul El-Sayed wants to reach <b>people aged 18 and older who live or have recently been in the United States</b>. This is information based on your Facebook profile and where you\'ve connected to the Internet.</span></div></div></div><div></div><div class=\"_4uor _52jw\"><div class=\"_5aj7\" id=\"u_19_3\"><div class=\"_4b").to_result(), Ok(TargetingParsed::ActivityOnTheFacebookFamily));


        assert_eq!(age_and_location("aged 18 and older who live or have recently been in the United States</b>. This is information based on your Facebook profile and where you\'ve connected to the Internet.</span></div></div></div><div></div><div class=\"_4uor _52jw\"><div class=\"_5aj7\" id=\"u_19_3\"><div class=\"_4b").to_result(), Ok(vec![
                    Some(TargetingParsed::Age("18 and older")),
                    Some(TargetingParsed::MinAge("18")),
                    Some(TargetingParsed::Region("the United States")),
                ]));


        assert_eq!(
            age_and_location(
                " ages 25 and older who live or were recently in the United States</b>",
            ),
            IResult::Done(
                "</b>",
                vec![
                    Some(TargetingParsed::Age("25 and older")),
                    Some(TargetingParsed::MinAge("25")),
                    Some(TargetingParsed::Region("the United States")),
                ]
            )
        );
        assert_eq!(
            advertiser_wants("including that World Wildlife Fund wants to reach",),
            IResult::Done(
                " wants to reach",
                TargetingParsed::Advertiser("World Wildlife Fund")
            )
        );
        assert_eq!(get_targeting("<div><div class='_4-i0 _26c5'><div class='clearfix'><div class='_51-u rfloat _ohf'><a class='_42ft _5upp _50zy layerCancel _51-t _50-0 _50z-' href='nullblank'>Close</a></div><div><h3 id='u_34_0' class='_52c9'>About This Facebook Ad</h3></div></div></div><div class='_4-i2 _pig _4s3a _50f4'><div class='_4uov'><div class='_4uoz'><div id='u_34_1'></div><div class='_3-8x'><span class='_4v6n'><div id='u_34_2'>One reason you're seeing this ad is that <b id='ad_prefs_advertiser'>Donald J. Trump</b> wants to reach <b>people who may be similar to their customers</b>. <a id='ad_prefs_link'>Learn more.</a></div></span><div class='_4hcd'><span>There may be other reasons you're seeing this ad, including that Donald J. Trump wants to reach <b>people who live in the United States</b>. This is information based on your Facebook profile and where you've connected to the internet.</span></div></div></div><div></div>"), IResult::Done("</b>. This is information based on your Facebook profile and where you\'ve connected to the internet.</span></div></div></div><div></div>", vec![TargetingParsed::Retargeting("people who may be similar to their customers"), TargetingParsed::Advertiser("Donald J. Trump"), TargetingParsed::Region("the United States")]));

        assert_eq!(get_targeting("<div><div class='_4-i0 _26c5'><div class='clearfix'><div class='_51-u rfloat _ohf'><a class='_42ft _5upp _50zy layerCancel _51-t _50-0 _50z-' href='nullblank'>Close</a></div><div><h3 id='u_o_0' class='_52c9'>About This Facebook Ad</h3></div></div></div><div class='_4-i2 _pig _4s3a _50f4'><div class='_4uov'><div class='_4uoz'><div id='u_o_1'></div><div class='_3-8x'><span class='_4v6n'><div id='u_o_2'>One reason you're seeing this ad is that <b id='ad_prefs_advertiser'>PayPal</b> wants to reach people interested in <b id='ad_prefs_interest'>Do it yourself (DIY)</b>, based on activity such as liking Pages or clicking on ads.</div></span><div class='_4hcd'><span>There may be other reasons you're seeing this ad, including that PayPal wants to reach <b>people ages 25 and older who were recently in the United States</b>. This is information based on your Facebook profile and where you've connected to the internet.</span></div></div></div>"), IResult::Done("</b>. This is information based on your Facebook profile and where you\'ve connected to the internet.</span></div></div></div>", vec![TargetingParsed::Advertiser("PayPal"), TargetingParsed::Age("25 and older"), TargetingParsed::MinAge("25"), TargetingParsed::Region("the United States")]));

        assert_eq!(get_targeting("<div><div class='_4-i0 _26c5'><div class='clearfix'><div class='_51-u rfloat _ohf'><a class='_42ft _5upp _50zy layerCancel _51-t _50-0 _50z-'>Close</a></div><div><h3 id='u_6u_0' class='_52c9'>About This Facebook Ad</h3></div></div></div><div class='_4-i2 _pig _4s3a _50f4'><div class='_4uov'><div class='_4uoz'><div id='u_6u_1'></div><div class='_3-8x'><span class='_4v6n'><div id='u_6u_2'>One reason you're seeing this ad is that <b id='ad_prefs_advertiser'>NRDC (Natural Resources Defense Council)</b> wants to reach <b>people who may be similar to their customers</b>. <a id='ad_prefs_link'>Learn more.</a></div></span><div class='_4hcd'><span>There may be other reasons you're seeing this ad, including that NRDC (Natural Resources Defense Council) wants to reach <b>people ages 18 and older</b>. This is information based on your Facebook profile and where you've connected to the internet.</span></div></div></div><div></div>").to_result(), Ok(vec![TargetingParsed::Retargeting("people who may be similar to their customers"), TargetingParsed::Advertiser("NRDC (Natural Resources Defense Council)"), TargetingParsed::Age("18 and older"), TargetingParsed::MinAge("18"),]));

        // TODO: implement engaged_with_content.
        // the problem is that it doesn't appear with the second strin g("There may be other reasons") from which we get second advertiser, gender, age and location.
        assert_eq!(get_targeting("<div><div class='_4-i0 _26c5'><div class='clearfix'><div class='_51-u rfloat _ohf'><a class='_42ft _5upp _50zy layerCancel _51-t _50-0 _50z-' href='nullblank'>Close</a></div><div><h3 id='u_1c_0' class='_52c9'>About this Facebook ad</h3></div></div></div><div class='_4-i2 _pig _4s3a _50f4'><div class='_4uov'><div class='_4uoz'><div id='u_1c_1'></div><div class='_3-8x'><span class='_4v6n'><div id='u_1c_2'>One reason why you're seeing this ad is because <b>International Rescue Committee</b> wants to reach people who have engaged with them or with their content.</div></span></div></div><div></div><div class='_4uor _52jw'><div class='_5aj7' id='u_1c_3'><div class='_4bl9'></div><div class='_4bl7 _5r7v'><i class='img sp_PVuC7pFCHqd sx_1baca1'><u>gear</u></i></div><div class='_4bl7'><a href='https://www.facebook.com/ads/preferences/'>Manage Your ad preferences</a></div></div></div>").to_result(), Ok(vec![TargetingParsed::Advertiser("International Rescue Committee"), TargetingParsed::EngagedWithContent]));

        assert_eq!(get_targeting("<div><div class='_4-i0 _26c5'><div class='clearfix'><div class='_51-u rfloat _ohf'><a class='_42ft _5upp _50zy layerCancel _51-t _50-0 _50z-' href='nullblank'>Close</a></div><div><h3 id='u_18_0' class='_52c9'>About This Facebook Ad</h3></div></div></div><div class='_4-i2 _pig _4s3a _50f4'><div class='_4uov'><div class='_4uoz'><div id='u_18_1'></div><div class='_3-8x'><span class='_4v6n'><div id='u_18_2'>One reason you're seeing this ad is that <b id='ad_prefs_advertiser'>Beto O'Rourke</b> wants to reach people based on their activity on the Facebook family of apps and services. This includes sharing links to their website, interacting with their content (such as clicking ads, watching videos or saving content) or directly interacting (such as messaging) with them.</div></span><div class='_4hcd'><span>There may be other reasons you're seeing this ad, including that Beto O'Rourke wants to reach <b>people ages 18 and older who live or were recently in Texas</b>. This is information based on your Facebook profile and where you've connected to the internet.</span></div></div></div><div></div><div class='_4uor _52jw'><div class='_5aj7' id='u_18_3'><div class='_4bl9'></div><div class='_4bl7 _5r7v'><i class='img sp_lXFspPUe6fO_2x sx_0ec8d2'><u>gear</u></i></div><div class='_4bl7'><a href='https://www.facebook.com/ads/preferences/'>Manage Your Ad Preferences</a></div></div></div><div class='mhs _1ray _1ra- _4uos'></div><div class='_4uou'><div class='_26c6 _3es4 fsl fwb fcb'>Tell Us What You Think</div><div><div class='clearfix _ikh' id='u_18_4'><div class='_4bl7 _3es3'>Was this explanation useful?</div><div class='_4bl7 _2pit'><a href='nullblank' id='u_18_5'>Yes</a></div><div class='_4bl9 _2pit'><a href='nullblank' id='u_18_6'>No</a></div></div><span class='hidden_elem _3es3' id='u_18_7'>Thanks for your rating.</span></div></div></div></div><div class='_5lnf uiOverlayFooter _5a8u _4866'><table class='uiGrid _51mz uiOverlayFooterGrid'><tbody><tr class='_51mx'><td class='_51m- prs uiOverlayFooterMessage'><a><i class='_4868 img sp_-wGGqwqeEPG_2x sx_a15d0e'><u>info</u></i><span class='_4867'>Learn more about Facebook Ads</span></a></td><td class='_51m- uiOverlayFooterButtons _51mw'></td></tr></tbody></table></div></div>").to_result(), Ok(vec![TargetingParsed::ActivityOnTheFacebookFamily, TargetingParsed::Advertiser("Beto O'Rourke"), TargetingParsed::Age("18 and older"), TargetingParsed::MinAge("18"), TargetingParsed::Region("Texas")]))
    }
}
