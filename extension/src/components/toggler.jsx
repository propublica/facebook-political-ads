import React from "react";
import { connect } from "react-redux";
import { toggle } from "actions.js";
import { withI18n } from "i18n.js";
import { countUnratedRatings } from "utils.js";
import { ToggleType } from "constants.js";
import { Ads, UnratedRatings } from "components/ads.jsx";

// Controls which section of tabs to show, defaults to the user's ads
export const Toggle = withI18n(
  ({ getMessage, type, message, active, amount, onToggleClick }) => (
    <div
      className={"toggle" + (active === type ? " active" : "")}
      onClick={function() {
        onToggleClick(type);
      }}
    >
      {getMessage(message)}
      {amount ? <b>{100 > amount ? amount : "100+"}</b> : ""}
    </div>
  )
);

// Our Main container.
export const TogglerUnconnected = ({ ads, ratings, active, onToggleClick }) => (
  <div id="toggler">
    <div id="tabs">
      <Toggle
        amount={countUnratedRatings(ratings)}
        active={active}
        message="rate_ads"
        onToggleClick={onToggleClick}
        type={ToggleType.RATER}
      />
      <Toggle
        amount={countUnratedRatings(ads)}
        active={active}
        message="see_ads"
        onToggleClick={onToggleClick}
        type={ToggleType.ADS}
      />
    </div>
    <div id="container">
      {active === ToggleType.ADS ? (
        <Ads ads={ads} />
      ) : (
        <UnratedRatings ratings={ratings} />
      )}
    </div>
  </div>
);
const togglerDispatchToProps = dispatch => ({
  onToggleClick: type => {
    dispatch(toggle(type));
  }
});
export const Toggler = connect(state => state, togglerDispatchToProps)(
  TogglerUnconnected
);
