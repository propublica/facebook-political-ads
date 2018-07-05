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

<div className='emoji' id={persona.img}></div>
   
    <div className="persona-info">
      <p>
        A {persona.age}-year-old { persona.gender} living in { persona.location} who is { persona.politics}.
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
