import React from "react";
import { connect } from "react-redux";
import { withI18n } from "i18n.js";

const YougovUnconnected = withI18n(
  ({ ygid }) =>
    ygid ? (
      <div id="ygidbar" style={{ padding: "3px" }}>
        Connected to YouGov. ID: {ygid}. <a href="">Help</a>
      </div>
    ) : (
      <div id="yougov-alert">
        You <span style={{ backgroundColor: "yellow" }}>need</span> to go to the
        YouGov page so we can get your ID, so you get your incentive for
        participation! You'll find the link in your email.
      </div>
    )
);
export const Yougov = connect(
  ({ ygid }) => ({
    ygid
  }),
  () => ({})
)(YougovUnconnected);
