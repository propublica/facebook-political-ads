import React from "react";
import { connect } from "react-redux";
import { Thanks } from "components/thanks.jsx";
import { Onboarding } from "components/onboarding.jsx";
import { acceptTerms, newAds } from "actions.js";
import { getAds } from "utils.js";

export const DispatcherUnconnected = ({ terms, language, onAcceptClick }) => {
  return (
    <div
      id="popup"
      lang={language.language}
      data-locale={`${language.language}_${language.country}`}
    >
      {terms ? (
        <Thanks />
      ) : (
        <Onboarding onAcceptClick={onAcceptClick(language)} />
      )}
    </div>
  );
};

const dispatchToProps = dispatch => ({
  onAcceptClick: language => e => {
    e.preventDefault();
    dispatch(acceptTerms());
    getAds(language, resp => dispatch(newAds(resp)));
  }
});

export const Dispatcher = connect(state => state, dispatchToProps)(
  DispatcherUnconnected
);
