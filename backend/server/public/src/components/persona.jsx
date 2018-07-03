import React from "react";
import { getAds, setPersona } from "actions.js";
import { connect } from "react-redux";

const Persona = ({ persona, setPersona, getAds }) => (
  <div
    className="persona"
    onClick={() => {
      setPersona(persona);
      getAds();
    }}
  >
    :)<br />
    {persona.name}
    <div className="persona-info">
      <p>
        Age: <span className="trait-category">{persona.age}</span>
      </p>
      <p>
        Gender: <span className="trait-category">{persona.gender}</span>
      </p>
      <p>
        Location:{" "}
        <span className="trait-category">
          {persona.location.city + ", " + persona.location.state}
        </span>
      </p>
      <p>
        Politics: <span className="trait-category">{persona.politics}</span>
      </p>
    </div>
  </div>
);

export default connect(
  () => ({}),
  dispatch => ({
    getAds: () => dispatch(getAds()),
    setPersona: persona => dispatch(setPersona(persona))
  })
)(Persona);
