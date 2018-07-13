import React from "react";
import Persona from "components/persona.jsx";
import { connect } from "react-redux";
import { getAds, setPersonaFacet } from "actions.js";
import Select from "react-select";

export class SelectorsAndPersonae extends React.Component {
  componentDidUpdate() {
    var el = document.getElementById("sticky_nav");
    var eloffset = el.offsetTop;

    window.onscroll = function() {
      var windowTop = window.scrollY;
      if (windowTop > eloffset) {
        el.classList.add("sticky");
      } else {
        el.classList.remove("sticky");
      }
    };
  }

  // See ads targeted to people who are: [ ] year olds, [women|men|any gender], any race, living in [any place] and politically [liberal|conservative|neither liberal nor conservative].
  render() {
    return (
      <div className="selectorsAndPersonae">
        <div className="selectorsOnly">
          <div id="sticky_nav">
            See ads targeted to people who are:{" "}
            <input
              type="number"
              value={
                this.props.persona === null || !this.props.persona.age
                  ? "--"
                  : this.props.persona.age
              }
              onChange={event => {
                this.props.setPersonaFacet("age", event.target.value);
                this.props.getAds();
              }}
            />{" "}
            years old,{" "}
            <select
              value={
                this.props.persona === null || !this.props.persona.gender
                  ? ""
                  : this.props.persona.gender
              }
              onChange={event => {
                this.props.setPersonaFacet("gender", event.target.value);
                this.props.getAds();
              }}
            >
              <option>any gender</option>
              <option>a woman</option>
              <option>a man</option>
            </select>, any race, in <br />
            <div
              className="select-container"
              style={{
                display: "inline-block",
                maxWidth: 400,
                minWidth: 300,
                maxHeight: "1.2em",
                input: {
                  height: "1em"
                }
              }}
            >
              <Select
                name="city"
                value={
                  this.props.persona && this.props.persona.location
                    ? this.props.persona.location.length === 2
                      ? this.props.persona.location[1] +
                        ", " +
                        this.props.persona.location[0]
                      : this.props.persona.location[0]
                    : "a city or any state"
                }
                noResultsText={
                  "No matching cities were targeted with many political ads."
                }
                placeholder={"a city or any state"}
                searchPromptText={"State or city"}
                style={{ width: "100%", container: { width: "100%" } }}
                onChange={event => {
                  this.props.setPersonaFacet(
                    "location",
                    event ? event.value.split(", ", 2).reverse() : null
                  );
                  this.props.getAds();
                }}
                options={cities_to_show.map(city => {
                  return { value: city, label: city };
                })}
              />
            </div>{" "}
            {/* <select
              value={
                this.props.persona && this.props.persona.location
                  ? this.props.persona.location[0]
                  : "any state"
              }
              onChange={event => {
                this.props.setPersonaFacet("location", [
                  event.target.value,
                  null
                ]);
                this.props.getAds();
              }}
            >
              <option>any state</option>
              <option>Alabama</option>
              <option>Alaska</option>
              <option>Arizona</option>
              <option>Arkansas</option>
              <option>California</option>
              <option>Colorado</option>
              <option>Connecticut</option>
              <option>Delaware</option>
              <option>District of Columbia</option>
              <option>Florida</option>
              <option>Georgia</option>
              <option>Hawaii</option>
              <option>Idaho</option>
              <option>Illinois</option>
              <option>Indiana</option>
              <option>Iowa</option>
              <option>Kansas</option>
              <option>Kentucky</option>
              <option>Louisiana</option>
              <option>Maine</option>
              <option>Maryland</option>
              <option>Massachusetts</option>
              <option>Michigan</option>
              <option>Minnesota</option>
              <option>Mississippi</option>
              <option>Missouri</option>
              <option>Montana</option>
              <option>Nebraska</option>
              <option>Nevada</option>
              <option>New Hampshire</option>
              <option>New Jersey</option>
              <option>New Mexico</option>
              <option>New York</option>
              <option>North Carolina</option>
              <option>North Dakota</option>
              <option>Ohio</option>
              <option>Oklahoma</option>
              <option>Oregon</option>
              <option>Pennsylvania</option>
              <option>Puerto Rico</option>
              <option>Rhode Island</option>
              <option>South Carolina</option>
              <option>South Dakota</option>
              <option>Tennessee</option>
              <option>Texas</option>
              <option>Utah</option>
              <option>Vermont</option>
              <option>Virginia</option>
              <option>Washington</option>
              <option>West Virginia</option>
              <option>Wisconsin</option>
              <option>Wyoming</option>
            </select>{" "} */}
            and politically{" "}
            <select
              value={
                this.props.persona === null ? "" : this.props.persona.politics
              }
              onChange={event => {
                this.props.setPersonaFacet("politics", event.target.value);
                this.props.getAds();
              }}
            >
              <option>neither liberal nor conservative</option>
              <option>conservative</option>
              <option>liberal</option>
            </select>.
          </div>
        </div>
        <div className="race-note">
          <p>
            Note: We don't let you filter by race because few political
            advertisers target using Facebook’s “multicultural affinity”
            category.
          </p>
        </div>

        <div>
          <p>Or see who is targeting a:</p>

          <ul className="flexy">
            {[
              {
                age: "72",
                gender: "a man",
                politics: "conservative",
                location: ["District of Columbia", "Washington"],
                pretty_location: "Washington, D.C.",
                img: "man-65"
              },
              {
                age: "40",
                gender: "a woman",
                politics: "liberal",
                location: ["California"],
                pretty_location: "California",
                img: "woman-40"
              },
              {
                age: "20",
                gender: "a man",
                politics: "neither liberal nor conservative",
                location: ["Wisconsin"],
                pretty_location: "Wisconsin",
                img: "man-20"
              },
              {
                age: "32",
                gender: "a woman",
                politics: "conservative",
                location: ["North Dakota"],
                pretty_location: "North Dakota",
                img: "woman-32"
              }
            ].map(persona => (
              <li key={persona.img} className="quarter">
                <Persona persona={persona} />
              </li>
            ))}
          </ul>
        </div>
      </div>
    );
  }
}

export default connect(
  ({ persona }) => ({ persona }),
  dispatch => ({
    getAds: () => dispatch(getAds()),
    setPersonaFacet: (facet_key, facet_val) =>
      dispatch(setPersonaFacet(facet_key, facet_val))
  })
)(SelectorsAndPersonae);

const cities_to_show = [
  "any state",
  "Alabama",
  "Alaska",
  "Arizona",
  "Arkansas",
  "California",
  "Colorado",
  "Connecticut",
  "Delaware",
  "District of Columbia",
  "Florida",
  "Georgia",
  "Hawaii",
  "Idaho",
  "Illinois",
  "Indiana",
  "Iowa",
  "Kansas",
  "Kentucky",
  "Louisiana",
  "Maine",
  "Maryland",
  "Massachusetts",
  "Michigan",
  "Minnesota",
  "Mississippi",
  "Missouri",
  "Montana",
  "Nebraska",
  "Nevada",
  "New Hampshire",
  "New Jersey",
  "New Mexico",
  "New York",
  "North Carolina",
  "North Dakota",
  "Ohio",
  "Oklahoma",
  "Oregon",
  "Pennsylvania",
  "Puerto Rico",
  "Rhode Island",
  "South Carolina",
  "South Dakota",
  "Tennessee",
  "Texas",
  "Utah",
  "Vermont",
  "Virginia",
  "Washington",
  "West Virginia",
  "Wisconsin",
  "Wyoming",

  "Washington, District of Columbia",
  "New York, New York",
  "Bellingham, Washington",
  "Baltimore, Maryland",
  "Seattle, Washington",
  "San Francisco, California",
  "Potomac, Maryland",
  "Los Angeles, California",
  "Columbus, Ohio",
  "Chicago, Illinois",
  "Houston, Texas",
  "Milwaukee, Wisconsin",
  "Raleigh, North Carolina",
  "Syracuse, New York",
  "Philadelphia, Pennsylvania",
  "Oakland, California",
  "Minneapolis, Minnesota",
  "Sacramento, California",
  "Sarasota, Florida",
  "Albuquerque, New Mexico",
  "Austin, Texas",
  "Oklahoma City, Oklahoma",
  "San Antonio, Texas",
  "Durham, North Carolina",
  "Alexandria, Virginia",
  "Fort Worth, Texas",
  "Portland, Oregon",
  "Alameda, California",
  "Eden Prairie, Minnesota",
  "Boulder, Colorado",
  "Toronto, Ontario",
  "St. Louis, Missouri",
  "Westport, New York",
  "Dallas, Texas",
  "Novato, California",
  "Farmingdale, New York",
  "Bradenton, Florida",
  "Ashland, Oregon",
  "Fargo, North Dakota",
  "Cedarburg, Wisconsin",
  "San Jose, California",
  "Albany, New York",
  "Plano, Texas",
  "Arlington, Virginia",
  "Bolton, Ontario",
  "Omaha, Nebraska",
  "Anaheim, California",
  "Tucson, Arizona",
  "Pacifica, California",
  "Binghamton, New York",
  "Southampton, Pennsylvania",
  "Columbia, Missouri",
  "Depoe Bay, Oregon",
  "Sugar Land, Texas",
  "Maryland Heights, Missouri",
  "Wasilla, Alaska",
  "San Diego, California",
  "Ann Arbor, Michigan",
  "Zionsville, Indiana",
  "Berkeley, California",
  "Madison, Wisconsin",
  "Murfreesboro, Tennessee",
  "Nashville, Tennessee",
  "Riverside, California",
  "Grayslake, Illinois",
  "Appleton, Wisconsin",
  "Cary, North Carolina",
  "West Chester, Pennsylvania",
  "Denver, Colorado",
  "Lincoln, Nebraska",
  "Greensboro, North Carolina",
  "Indianapolis, Indiana",
  "Leesburg, Virginia",
  "Charlotte, North Carolina",
  "Urbana, Illinois",
  "Charlottesville, Virginia",
  "Belmont, California",
  "Boston, Massachusetts",
  "Atlanta, Georgia",
  "Newcastle, California",
  "Suffolk County, New York",
  "Stanford, California",
  "Pittsburgh, Pennsylvania",
  "Mount Clemens, Michigan",
  "Kingston, Ontario",
  "Peoria, Illinois",
  "Santa Ana, California",
  "Crofton, Maryland",
  "Sunnyvale, California",
  "Cerritos, California",
  "White Hall, Virginia",
  "Streamwood, Illinois",
  "Newark, New Jersey",
  "New Orleans, Louisiana",
  "Phoenix, Arizona",
  "Aurora, Colorado",
  "Somerville, Massachusetts",
  "Silver Spring, Maryland",
  "Missoula, Montana",
  "Burlington, Vermont",
  "Allentown, Pennsylvania",
  "Red Oak, Iowa",
  "Oberlin, Ohio",
  "Dearborn, Michigan",
  "Canonsburg, Pennsylvania",
  "Roswell, Georgia",
  "Garland, Texas",
  "Tripoli, Iowa",
  "Nantucket, Massachusetts",
  "Burlingame, California",
  "Plattsburgh, New York",
  "State College, Pennsylvania",
  "Jackson, Wyoming",
  "Longmont, Colorado",
  "Portland, Maine",
  "Franklin, Kentucky",
  "Lawton, Michigan",
  "Cumberland, Virginia",
  "York, Pennsylvania",
  "Nassau County, New York",
  "Newtown, Bucks County, Pennsylvania",
  "Paris, Île-de-France",
  "Saint Paul, Minnesota",
  "Salt Lake City, Utah",
  "Athens, Georgia",
  "Des Moines, Iowa",
  "Hardwick, Georgia",
  "Rochester, New York",
  "Ashfield, Massachusetts",
  "Mehlville, Missouri",
  "Lincoln City, Oregon",
  "New Providence, New Jersey",
  "Edmonton, Alberta",
  "Fuquay-Varina, North Carolina",
  "Colorado Springs, Colorado",
  "Buena Park, California",
  "Broken Bow, Oklahoma",
  "Santa Monica, California",
  "Bristow, Virginia",
  "Raytown, Missouri",
  "Chico, California",
  "Rialto, California",
  "Cincinnati, Ohio",
  "Clinton, Connecticut",
  "Leamington, Ontario",
  "South Laurel, Maryland",
  "Lakewood, Ohio",
  "Justiceburg, Texas",
  "Jackson, Michigan",
  "Belle View, Virginia",
  "Harwinton, Connecticut",
  "Gulfport, Florida",
  "Wahpeton, North Dakota",
  "Greenfield, Massachusetts",
  "Champaign, Illinois",
  "Garden Grove, California",
  "Holly Springs, North Carolina",
  "Redland, Maryland",
  "Tempe, Arizona",
  "Haverford, Pennsylvania",
  "Olympia, Washington",
  "Cambridge, Massachusetts",
  "Davis, California",
  "Santa Fe, New Mexico",
  "Burien, Washington",
  "Provo, Utah",
  "Bluffton, Indiana",
  "Bethesda, Maryland",
  "Las Vegas, Nevada",
  "Carmel, California",
  "Lafayette, California",
  "Washington Four, Maryland"
];
