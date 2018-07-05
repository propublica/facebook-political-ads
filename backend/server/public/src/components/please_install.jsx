import React from "react";
import { connect } from "react-redux";

const PleaseInstallUnconnected = ({ persona }) =>
  persona &&
  persona.location &&
  ([
    "TX",
    "IA",
    "VA",
    "NC",
    "WV",
    "AZ",
    "IL",
    "FL",
    "PA",
    "CN",
    "OH",
    "TN",
    "IN",
    "KY",
    "UT",
    "AL",
    "LA",
    "GA",
    "NV",
    "SD",
    "DE",
    "NH",
    "VT",
    "AR",
    "HI",
    "ID",
    "SC",
    "RI",
    "KS"
  ].indexOf(persona.location[0]) > -1 ||
    persona.politics == "conservative") ? (
      <div className="glad-you-here">
        <div>
          <img
            src="https://propublica.s3.amazonaws.com/assets/images/headshot.jpg"
            alt="headshot"
          />
        </div>
        <div>
          <h2>Is this you? We need your help!</h2>
          <h4>
          Not many people with the traits you selected are using our political
          ad collector.
          </h4>
          <p>
          This means even though the ads are out there, we don't have them in
          our database. Is this you? Help us help other people like you by using
          the ad collector.
          </p>
          <a href="#">Use the ad collector</a>
        </div>
      </div>
    ) : (
      <div className="glad-you-here">
        <div>
        <img
            src="https://propublica.s3.amazonaws.com/assets/images/headshot.jpg"
            alt="headshot"
        />
        </div>
      <div>
        <h2>Hey, we're glad you're here!</h2>
          <h4>You can help us make our ad database more representative.</h4>
        <p>
          The political ad collector only receives Facebook ads from people
          using the browser extension. Help us help other people like you by
          using the ad collector.
          </p>
          <a href="#">Use the ad collector</a>
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
