import React from "react";
import { connect } from "react-redux";
import { withI18n } from "i18n.js";
import { Toggler } from "components/toggler.jsx";

const ThanksUnconnected = withI18n(
  ({ getMessage, thanks, ratings_count, ygid }) => (
    <div id="thankscontainer">
      <div id="ygidbar">Your YouGov ID: {ygid}</div>
      <div id="thanksbar">
        {thanks ? getMessage(thanks, { count: ratings_count || 0 }) : null}
      </div>
      <Toggler />
    </div>
  )
);
export const Thanks = connect(state => state)(ThanksUnconnected);
