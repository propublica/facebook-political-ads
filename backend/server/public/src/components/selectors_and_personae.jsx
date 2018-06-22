import React from "react";
import Persona from "components/persona.jsx";

const FbpacFeltrons = () => (
  <div>
    <p>
      Fill out your infomration to see who is targeting a 48 year old male
      living in Ohio and who is conservative.
    </p>
    <div>
      <p>Or see who is targeting someone like</p>
      <ul>
        {["Donald J. Trump", "Barack Obama", "Someoen Else"].map(name => (
          <li key={name}>
            <Persona name={name} />
          </li>
        ))}
      </ul>
    </div>
  </div>
);

export default FbpacFeltrons;
