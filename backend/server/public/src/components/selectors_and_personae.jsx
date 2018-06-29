import React from "react";
import Persona from "components/persona.jsx";

const SelectorsAndPersonae = () => (
  <div className="selectorsAndPersonae">
    <p>
      Fill out your infomration to see who is targeting a{" "}
      <select>
        <option>48 year old</option>{" "}
      </select>
      <select>
        <option>male</option>{" "}
      </select>
      living in{" "}
      <select>
        <option>Ohio</option>
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
          "Donald J. Trump",
          "Barack Obama",
          "Billy Joel",
          "Warren Buffett"
        ].map(name => (
          <li key={name} className="quarter">
            <Persona name={name} />
          </li>
        ))}
      </ul>
    </div>
  </div>
);

export default SelectorsAndPersonae;
