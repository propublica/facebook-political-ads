import React from "react";
import { connect } from "react-redux";
import { newSearch } from "actions.js";

let Term = ({ search, term, dispatch }) => (
  <li>
    <button
      type="button"
      className={term === search ? "prefab current" : "prefab"}
      onClick={() => dispatch(newSearch(term))}
      value={term}
    >
      {term}
    </button>
  </li>
);
export default connect()(Term);
