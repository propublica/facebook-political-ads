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
        <strong>{ persona.age }</strong>-year-old <strong>{ persona.gender }</strong> living in <strong>{ persona.pretty_location }</strong> who is <strong>{ persona.politics }</strong>.
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
