import React from "react";
import Persona from "components/persona.jsx";
import { connect } from "react-redux";
import { getAdsByBucket, setPersonaFacet } from "actions.js";

const SelectorsAndPersonae = ({ getAdsByBucket, setPersonaFacet, persona }) => (
  <div className="selectorsAndPersonae">
    <p>
      Fill out your information to see who is targeting a{" "}
      <select>
        <option>48 year old</option>{" "}
        {/* when persona changes, shoudl change these dropdowns */}
      </select>{" "}
      <select>
        <option>male</option>
      </select>{" "}
      living in{" "}
      <select>
        <option>Ohio</option>
        <option>Long Island, NY</option>
        <option>big city</option>
        <option>suburbs</option>
      </select>{" "}
      and who is{" "}
      <select>
        <option>conservative</option>
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
    getAdsByBucket: () => dispatch(getAdsByBucket()),
    setPersonaFacet: (facet_key, facet_val) =>
      dispatch(setPersonaFacet(facet_key, facet_val))
  })
)(SelectorsAndPersonae);
