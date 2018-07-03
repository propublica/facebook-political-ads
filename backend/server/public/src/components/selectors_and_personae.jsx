import React from "react";
import Persona from "components/persona.jsx";
import { connect } from "react-redux";
import { getAds, setPersonaFacet } from "actions.js";

const SelectorsAndPersonae = ({ getAds, setPersonaFacet, persona }) => (
  <div className="selectorsAndPersonae">
    <p>
      Fill out your information to see who is targeting a{" "}
      <input
        type="number"
        value={persona === null ? "" : persona.age}
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
      living in{" "}
      <select
        value={persona === null ? "" : persona.location}
        onChange={event => {
          setPersonaFacet("location", event.target.value);
          getAds();
        }}
      >
        <option>Ohio</option>
        <option>Long Island, NY</option>
        <option>big city</option>
        <option>suburbs</option>
        <option>rural America</option>
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
    <div>
      <p>Or see who is targeting someone like:</p>
      <ul className="flexy">
        {[
          {
            age: "65 or older",
            gender: "men",
            politics: "conservative",
            location: {
              city: "Washington",
              state: "DC" // okay okay it's not a state, geez.
            },
            name: "Donald J. Trump"
          },
          {
            age: "56",
            gender: "men",
            politics: "liberal",
            location: {
              city: "Washington",
              state: "DC" // okay okay it's not a state, geez.
            },
            name: "Barack Obama"
          },
          {
            age: "69",
            gender: "men",
            politics: "apolitical",
            location: {
              cities: "Long Island",
              state: "NY"
            },
            name: 'William "Billy" Joel'
          },
          {
            age: "28",
            gender: "women",
            politics: "apolitical",
            location: {
              state: "TN"
            },
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
