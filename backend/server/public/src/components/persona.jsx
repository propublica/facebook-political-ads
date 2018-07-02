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
  </div>
);

export default connect(
  () => ({}),
  dispatch => ({
    getAds: () => dispatch(getAds()),
    setPersona: persona => dispatch(setPersona(persona))
  })
)(Persona);
