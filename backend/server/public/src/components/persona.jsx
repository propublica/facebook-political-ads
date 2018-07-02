import React from "react";
import { getAdsByBucket, setPersona } from "actions.js";
import { connect } from "react-redux";

const Persona = ({ persona, setPersona, getAdsByBucket }) => (
  <div
    className="persona"
    onClick={() => {
      setPersona(persona);
      getAdsByBucket();
    }}
  >
    :)<br />
    {persona.name}
  </div>
);

export default connect(
  () => ({}),
  dispatch => ({
    getAdsByBucket: () => dispatch(getAdsByBucket()),
    setPersona: persona => dispatch(setPersona(persona))
  })
)(Persona);
