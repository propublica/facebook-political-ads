import React from "react";
import { connect } from "react-redux";
import { withI18n } from "i18n.js";

const YougovUnconnected = withI18n(
  ({ ygid }) =>
    ygid ? (
      <div id="ygidbar" style={{ padding: "3px" }}>
        Connected to YouGov. ID: {ygid}.{" "}
        <a href="https://projects.propublica.org/facebook-ads/yghelp">Help</a>
      </div>
    ) : (
      <div id="yougov-alert">
        Please go to the YouGov page for this project. It will let our systems
        pull in your ID to make sure you are compensated for your participation.
        You'll find the link in your email.
      </div>
    )
);
export const Yougov = connect(
  ({ ygid }) => ({
    ygid
  }),
  () => ({})
)(YougovUnconnected);
