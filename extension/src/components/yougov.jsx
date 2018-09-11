import React from "react";
import { connect } from "react-redux";
import { withI18n } from "i18n.js";

const YougovUnconnected = withI18n(({ ygid }) => (
  <div id="ygidbar" style={{  padding: "3px;" }}>
    Connected to YouGov. ID: {ygid}
  </div>
));
export const Yougov = connect(state => state)(YougovUnconnected);
