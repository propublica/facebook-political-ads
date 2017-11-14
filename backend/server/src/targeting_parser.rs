use errors::*;
use nom::{IResult, rest_s};

#[derive(Clone, Debug, PartialEq)]
pub enum Targeting<'a> {
    Gender(&'a str),
    City(&'a str),
    State(&'a str),
    Region(&'a str),
    Age(&'a str),
    Interest(&'a str),
    Segment(&'a str),
}

named!(interest(&str) -> Vec<Targeting>,
    do_parse!(
        interest: rest_s >>
        (vec![Targeting::Interest(interest)])
    )
);

named!(segment(&str) -> Vec<Targeting>,
    do_parse!(
        tag!("\"") >>
        segment: take_until!("\"") >>
        tag!("\"") >>
        (vec![Targeting::Segment(segment)])
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
        complete: ws!(take_until_and_consume!(" who")) >>
        (Some(Targeting::Age(complete)))
    )
);

named!(country(&str) -> Vec<Option<Targeting>>,
    do_parse!(country: rest_s >> (vec![Some(Targeting::Region(country))]))
);

named!(city_state(&str) -> Vec<Option<Targeting>>,
    do_parse!(
        ws!(alt!(take_until_and_consume!("near") | take_until_and_consume!("in"))) >>
        city: ws!(take_until_and_consume!(",")) >>
        state: rest_s >>
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

named!(targeting(&str) -> Vec<Targeting>,
    alt!(gender_age_and_location | segment | interest)
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

            IResult::Done("", vec![Targeting::Segment("Generation X")])
        );
        let i = "American Federation of State, County and Municipal Employees";
        assert_eq!(interest(i), IResult::Done("", vec![Targeting::Interest(i)]));
        assert_eq!(
            gender("men"),
            IResult::Done("", Some(Targeting::Gender("Men")))
        );
        assert_eq!(
            gender("women"),
            IResult::Done("", Some(Targeting::Gender("Women")))
        );
        assert_eq!(gender("people"), IResult::Done("", None));
        assert_eq!(
            age(" ages 26 to 62 who "),
            IResult::Done("", Some(Targeting::Age("26 to 62")))
        );
        assert_eq!(
            country("United States"),
            IResult::Done("", vec![Some(Targeting::Region("United States"))])
        );
        assert_eq!(
            city_state("near Burlington, North Carolina"),
            IResult::Done(
                "",
                vec![
                    Some(Targeting::City("Burlington")),
                    Some(Targeting::State("North Carolina")),
                ],
            )
        );
        assert_eq!(
            targeting(
                "people ages 18 and older who live or were recently near San Francisco, California",
            ),
            IResult::Done(
                "",
                vec![
                    Targeting::Age("18 and older"),
                    Targeting::City("San Francisco"),
                    Targeting::State("California"),
                ],
            )
        );
    }

    #[test]
    fn test_all() {}
}
