use nom::{IResult, alpha, rest, rest_s, space};

#[derive(Clone)]
pub enum Targeting<'a> {
    Gender(&'a str),
    City(&'a str),
    State(&'a str),
    Country(&'a str),
    Age(&'a str),
    Interest(&'a str),
    Segment(&'a str),
}

named!(segment(&str) -> Vec<Targeting>,
    do_parse!(
        segment: rest_s >>
        (vec![Targeting::Segment(segment)])
    )
);

named!(interest(&str) -> Vec<Targeting>,
    do_parse!(
        tag!("\"") >>
        interest: take_until!("\"") >>
        tag!("\"") >>
        (vec![Targeting::Segment(interest)])
    )
);

named!(gender(&str) -> Option<Targeting>,
    alt!(
        tag!("men") => {|_| Some(Targeting::Gender("Men")) } |
        tag!("women") => {|_| Some(Targeting::Gender("Women")) } |
        tag!("people") => {|_| None }
    )
);

named!(age(&str) -> Option<Targeting>,
    do_parse!(
        ws!(tag!("age")) >>
        ws!(opt!(tag!("s"))) >>
        complete: ws!(take_until!("who")) >>
        (Some(Targeting::Age(complete)))
    )
);

named!(country(&str) -> Vec<Option<Targeting>>,
    do_parse!(country: alpha >> (vec![Some(Targeting::Country(country))]))
);

named!(city_state(&str) -> Vec<Option<Targeting>>,
    do_parse!(
        ws!(alt!(take_until!("near") | take_until!("in"))) >>
        city: ws!(take_until!(",")) >>
        state: alpha >>
        (vec![Some(Targeting::City(city)), Some(Targeting::State(state))])
    )
);

named!(gender_age_and_location(&str) -> Vec<Targeting>,
    do_parse!(
        g: ws!(gender) >>
        a: ws!(age) >>
        l: ws!(alt!(city_state | country)) >>
        ([&vec![g, a][..], &l[..]].concat().into_iter().filter_map(|x| x).collect())
    )
);

named!(parse_targeting(&str) -> Vec<Targeting>,
    alt!(gender_age_and_location | interest | segment)
);
