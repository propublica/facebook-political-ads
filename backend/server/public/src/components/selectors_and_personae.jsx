import React from "react";
import Persona from "components/persona.jsx";
import { connect } from "react-redux";
import { getAds, setPersonaFacet } from "actions.js";

const SelectorsAndPersonae = ({ getAds, setPersonaFacet, persona }) => (
  <div className="selectorsAndPersonae">
    <div className="selectorsOnly">
      <p>
        Fill out your information to see who is targeting a{" "}
        <input
          type="number"
          value={persona === null ? "28" : persona.age}
          onChange={event => {
            setPersonaFacet("age", event.target.value);
            getAds();
          }}
        />{" "}
        year old{" "}
        <select
          value={persona === null ? "" : persona.gender}
          onChange={event => {
            setPersonaFacet("gender", event.target.value);
            getAds();
          }}
        >
          <option>female</option>
          <option>male</option>
          <option>person</option>
        </select>{" "}
      </p>
      <p>
        of any race living in{" "}
        <select
          value={persona === null ? "" : persona.location[0]}
          onChange={event => {
            setPersonaFacet("location", [event.target.value, null]);
            getAds();
          }}
        >
          <option>Alabama</option>
          <option>Alaska</option>
          <option>Arizona</option>
          <option>Arkansas</option>
          <option>California</option>
          <option>Colorado</option>
          <option>Connecticut</option>
          <option>Delaware</option>
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
        </select>{" "}
        and who is{" "}
        <select
          value={persona === null ? "" : persona.politics}
          onChange={event => {
            setPersonaFacet("politics", event.target.value);
            getAds();
          }}
        >
          <option>conservative</option>
          <option>liberal</option>
          <option>apolitical</option>
        </select>.
      </p>

      <div className="race-note">
        <p>
          Note: We don't let you filter by race because few advertisers target
          using Facebook’s “multicultural affinity” category.
        </p>
      </div>
    </div>
    <div>
      <p className="personaIntro">Or see who is targeting someone like:</p>
      <ul className="flexy">
        {[
          {
            age: "65 or older",
            gender: "male",
            politics: "conservative",
            location: ["DC", "Washington"],
            name: "Donald J. Trump"
          },
          {
            age: "56",
            gender: "male",
            politics: "liberal",
            location: ["DC", "Washington"],
            name: "Barack Obama"
          },
          {
            age: "69",
            gender: "male",
            politics: "apolitical",
            location: ["NY", "Long Island"],
            name: 'William "Billy" Joel'
          },
          {
            age: "28",
            gender: "female",
            politics: "apolitical",
            location: ["TN", "Nashville"],
            name: "Taylor Swift"
          }
        ].map(persona => (
          <li key={persona.name} className="quarter">
            <Persona persona={persona} />
          </li>
        ))}
      </ul>
    </div>
  </div>
);

export default connect(
  ({ persona }) => ({ persona }),
  dispatch => ({
    getAds: () => dispatch(getAds()),
    setPersonaFacet: (facet_key, facet_val) =>
      dispatch(setPersonaFacet(facet_key, facet_val))
  })
)(SelectorsAndPersonae);
