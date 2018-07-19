import React from "react";
import { connect } from "react-redux";

const PleaseInstallUnconnected = ({ persona }) =>
  persona &&
  persona.location &&
  ([
    "NC",
    "PA",
    "TX",
    "CT",
    "MI",
    "IL",
    "WY",
    "FL",
    "MT",
    "NJ",
    "CO",
    "AZ",
    "OK",
    "IA",
    "GA",
    "OH",
    "DE",
    "UT",
    "IN",
    "TN",
    "AL",
    "KY",
    "LA",
    "NV",
    "SD",
    "ID",
    "HI",
    "AR",
    "RI",
    "KS",
    "SC",
    "MS"
  ].indexOf(persona.location[0]) > -1 ||
    persona.politics == "conservative") ? (
    <div className="glad-you-here">
      <div className="wave" />
      <div>
        <h2>Is this you? We need your help!</h2>
        <h4>
          Not many people with the traits you selected are using our Political
          Ad Collector browser plug-in.
        </h4>
        <p>
          That means that even though ads targeting people like you are out
          there, we don’t have them in our database. Is this you? If this
          describes you, help us track more ads by using the ad collector.
        </p>
        <a href="https://projects.propublica.org/political-ad-collector/">
          Install the ad collector
          </a>
      </div>
    </div>
  ) : (
    <div className="glad-you-here">
        <div className="wave" />
        <div>
          <h2>Hey, we’re glad you’re here!</h2>
        <h4>You can help us make our ad database more representative.</h4>
          <p>
          The Political Ad Collector only receives Facebook ads from people using the browser plug-in. Help us track more ads by using the ad collector.
        </p>
        <a href="https://projects.propublica.org/political-ad-collector/">
          Install the ad collector
          </a>
        </div>
    </div>
  );

const PleaseInstall = connect(
  ({ persona }) => ({
    persona
  }),
  () => ({})
)(PleaseInstallUnconnected);
export default PleaseInstall;
