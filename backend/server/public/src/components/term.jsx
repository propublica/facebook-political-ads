import React from "react";
import { connect } from "react-redux";
import { fetchSearch } from "actions.js";

export const TermUnconnected = ({ search, term, newSearch }) => (
  <li>
    <button
      type="button"
      className={term === search ? "prefab current" : "prefab"}
      onClick={() => newSearch(term)}
      value={term}
    >
      {term}
    </button>
  </li>
);
const Term = connect(
  () => ({}),
  dispatch => ({
    newSearch: term => dispatch(fetchSearch(term))
  })
)(TermUnconnected);
export default Term;
