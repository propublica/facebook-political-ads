import React from "react";
import { connect } from "react-redux";
import { Thanks } from "components/thanks.jsx";
import { Onboarding } from "components/onboarding.jsx";
import { acceptTerms, newAds } from "actions.js";
import { getAds } from "utils.js";
import { Yougov } from "components/yougov.jsx";

export const DispatcherUnconnected = ({
  terms,
  language,
  onAcceptClick,
  ygid
}) => {
  return (
    <div
      id="popup"
      lang={language.language}
      data-locale={`${language.language}_${language.country}`}
    >
      {ygid ? (
        terms ? (
          <Thanks />
        ) : (
          <Onboarding onAcceptClick={onAcceptClick(language)} />
        )
      ) : (
        <Yougov />
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
