import React from "react";
import { render } from "react-dom";
import thunkMiddleware from "redux-thunk";
import { applyMiddleware, compose, combineReducers, createStore } from "redux";
import { Provider, connect } from "react-redux";
import persistState from "redux-localstorage";
import { createLogger } from "redux-logger";
import {
  sendAds,
  getAds,
  updateBadge,
  adForRequest,
  getBrowserLocale,
  mergeAds
} from "utils.js";
import langs from "langs";
import countries from "i18n-iso-countries";
import { withI18n, activeCountries, activeLanguages } from "./i18n";

// styles
import "../css/styles.css";

// Constants
const ToggleType = {
  ADS: "ads",
  RATER: "rater"
};

const RatingType = {
  POLITICAL: "political",
  NORMAL: "normal"
};

// Action Types
const ACCEPT_TERMS = "accept_terms";
const TOGGLE_TAB = "toggle_tab";
const NEW_ADS = "new_ads";
const NEW_RATINGS = "new_ratings";
const UPDATE_AD = "update_ad";
const UPDATE_RATING = "update_rating";
const SET_LANGUAGE = "set_language";
const SET_COUNTRY = "set_country";
const SAY_THANKS = "say_thanks";

// Actions
const setLanguage = language => ({ type: SET_LANGUAGE, language });
const setCountry = country => ({ type: SET_COUNTRY, country });
const acceptTerms = () => ({ type: ACCEPT_TERMS });
const toggle = value => ({ type: TOGGLE_TAB, value });
const newAds = ads => ({
  type: NEW_ADS,
  value: ads
});
const newRatings = ratings => ({
  type: NEW_RATINGS,
  value: ratings
});
const updateAd = (id, rating) => ({
  type: UPDATE_AD,
  id: id,
  value: rating
});
const updateRating = (id, rating) => ({
  type: UPDATE_RATING,
  id: id,
  value: rating
});
const sayThanks = ratings_count => ({
  type: SAY_THANKS,
  ratings_count
});
const rateAd = (ad, rating, update) => {
  return dispatch => {
    let body = {
      ...adForRequest(ad),
      political: rating === RatingType.POLITICAL
    };
    dispatch(update(ad.id, rating));
    dispatch(sayThanks(store.getState().ratings_count || 0));
    let cb = () => ({});
    return sendAds([body], store.getState().language).then(cb, cb);
  };
};

// Reducers
const active = (state = ToggleType.RATER, action) => {
  switch (action.type) {
    case TOGGLE_TAB:
      return action.value;
    default:
      return state;
  }
};

const buildUpdate = type => (state = [], action) => {
  switch (action.type) {
    case "new_" + type + "s":
      return mergeAds(state, action.value);
    case "update_" + type:
      return mergeAds(
        state.map(ad => {
          if (ad.id === action.id) {
            return { ...ad, rating: action.value };
          }
          return ad;
        }),
        []
      );
    default:
      return state;
  }
};

const terms = (state = false, action) => {
  switch (action.type) {
    case ACCEPT_TERMS:
      return true;
    default:
      return state;
  }
};

const thanks_messages = [
  "thanks1", // "Thanks! 🙂",
  "thanks2", // "Keep it up!",
  "thanks3", // "We couldn't do it without you!",
  "thanks4", // "Thanks for helping out!",
  "thanks5" //  "Way to go! You've categorized $count$ ads so far."
];
const thanks = (state = null, action) => {
  switch (action.type) {
    case SAY_THANKS:
      // we thank you frequently at first and exponentially less often later
      // for instance, your first ad, we have a 50% chance of thanking you
      // and your 4th ad a 25% chance and so on.
      let message =
        Math.random() < 1 / (2 * Math.sqrt(action.ratings_count || 1))
          ? thanks_messages[parseInt(Math.random() * thanks_messages.length)]
          : null;
      return message;
    default:
      return state;
  }
};
const ratings_count = (state = 0, action) => {
  switch (action.type) {
    case UPDATE_RATING:
      return state + 1;
    default:
      return state;
  }
};

const browserLocale = getBrowserLocale();
const language = (state = browserLocale, action) => {
  switch (action.type) {
    case SET_LANGUAGE:
      return { ...state, language: action.language };
    case SET_COUNTRY:
      return { ...state, country: action.country };
    default:
      return state;
  }
};

// The main reducer!
const reducer = combineReducers({
  active,
  ads: buildUpdate("ad"),
  ratings: buildUpdate("rating"),
  terms,
  thanks,
  language,
  ratings_count
});

let middleware = [thunkMiddleware];
if (process.env.NODE_ENV === "development") {
  middleware.push(createLogger());
}
const enhancer = compose(...[persistState(), applyMiddleware(...middleware)]);
let store = createStore(reducer, enhancer);

// Ad utilities
const getUnratedRatings = ratings =>
  ratings.filter(
    rating => rating.rating === RatingType.POLITICAL || !("rating" in rating)
  );

const countUnratedRatings = ratings =>
  ratings.filter(rating => !("rating" in rating)).length;

let div = document.createElement("div");
const query = (html, selector) => {
  div.innerHTML = html;
  return div.querySelector(selector);
};

const getImage = html => {
  let img = query(html, "img");
  if (img) return img.getAttribute("src");
};

const getAdMessage = html => {
  let p =
    query(html, ".userContent p") || query("div.mbs") || query(html, "span");
  if (p) return p.innerHTML;
};

const getTitle = html => {
  let a =
    query(html, "h5 a") ||
    query(html, "h6 a") ||
    query(html, "strong") ||
    query(html, "span.fsl");
  if (a) return a.innerText;
};

const insertAdFields = ads =>
  ads.map(ad => ({
    ...ad,
    image: getImage(ad.html),
    message: getAdMessage(ad.html),
    title: getTitle(ad.html),
    raw: ad.html
  }));

// Views
const Ad = ({ id, raw }) => (
  <div className="ad" id={id}>
    <div className="ad-display" dangerouslySetInnerHTML={{ __html: raw }} />
  </div>
);

const RatingForm = withI18n(({ getMessage, rating, action, question }) => (
  <div className="rater">
    {getMessage(question)}
    <button
      id={"political" + rating.id}
      onClick={function() {
        return action(rating, RatingType.POLITICAL);
      }}
    >
      {getMessage("political")}
    </button>
    <button
      id={"normal" + rating.id}
      onClick={function() {
        return action(rating, RatingType.NORMAL);
      }}
    >
      {getMessage("normal")}
    </button>
  </div>
));

// Ads to be rated and sent to the server
const Rating = withI18n(({ getMessage, rating, action, question }) => (
  <div className="rating">
    {"rating" in rating ? (
      <b className="political">{getMessage("political")}</b>
    ) : (
      <RatingForm action={action} rating={rating} question={question} />
    )}
    <Ad
      title={rating.title}
      message={rating.message}
      id={rating.id}
      image={rating.image}
      raw={rating.raw}
    />
  </div>
));

const Ratings = ({ onRatingClick, ratings }) => (
  <div id="ratings">
    {ratings.map(rating => (
      <Rating
        key={rating.id}
        rating={rating}
        action={onRatingClick}
        question="rating_question"
      />
    ))}
  </div>
);
const ratingsStateToProps = state => ({
  ratings: insertAdFields(getUnratedRatings(state.ratings))
});
const ratingsDispatchToProps = dispatch => ({
  onRatingClick: (id, rating) => {
    dispatch(rateAd(id, rating, updateRating));
  }
});
const UnratedRatings = connect(ratingsStateToProps, ratingsDispatchToProps)(
  Ratings
);

// Ads from the server to show
let Ads = ({ ads, onAdClick }) => (
  <div id="ads">
    {ads.map(ad => (
      <Rating
        key={ad.id}
        rating={ad}
        action={onAdClick}
        question="verify_question"
      />
    ))}
  </div>
);
const adStateToProps = state => ({
  ads: insertAdFields(getUnratedRatings(state.ads))
});
const adDispatchToProps = dispatch => ({
  onAdClick: (id, rating) => {
    dispatch(rateAd(id, rating, updateAd));
  }
});
Ads = connect(adStateToProps, adDispatchToProps)(Ads);

// Controls which section of tabs to show, defaults to the
const Toggle = withI18n(
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
let Toggler = ({ ads, ratings, active, onToggleClick }) => (
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
Toggler = connect(state => state, togglerDispatchToProps)(Toggler);

let ContainerWithThanks = withI18n(({ getMessage, thanks, ratings_count }) => (
  <div id="thankscontainer">
    <div id="thanksbar">
      {thanks ? getMessage(thanks, { count: ratings_count || 0 }) : null}
    </div>
    <Toggler />
  </div>
));
ContainerWithThanks = connect(state => state)(ContainerWithThanks);

let SelectLanguage = ({ language, onChange }) => {
  const allLangs = langs.all();

  const createOption = lang => (
    <option key={lang["1"]} value={lang["1"]}>
      {lang["name"]} / {lang["local"]}
    </option>
  );

  return (
    <select value={language} onChange={onChange}>
      {allLangs.filter(l => activeLanguages.includes(l["1"])).map(createOption)}
      <option disabled>──────────</option>
      {allLangs
        .filter(l => !activeLanguages.includes(l["1"]))
        .map(createOption)}
    </select>
  );
};
const selectLanguageDispatchToProps = dispatch => ({
  onChange: e => {
    dispatch(setLanguage(e.target.value));
  }
});
SelectLanguage = connect(
  state => state.language,
  selectLanguageDispatchToProps
)(SelectLanguage);

let SelectCountry = ({ language, country, onChange }) => {
  const lang = countries.langs().includes(language) ? language : "en";
  const names = countries.getNames(lang);
  const keys = Object.keys(names);

  const createOption = alpha2 => (
    <option key={alpha2} value={alpha2}>
      {names[alpha2]}
    </option>
  );

  return (
    <select value={country} onChange={onChange}>
      {keys.filter(k => activeCountries.includes(k)).map(createOption)}
      <option disabled>──────────</option>
      {keys.filter(k => !activeCountries.includes(k)).map(createOption)}
    </select>
  );
};
const selectCountryDispatchToProps = dispatch => ({
  onChange: e => {
    dispatch(setCountry(e.target.value));
  }
});
SelectCountry = connect(state => state.language, selectCountryDispatchToProps)(
  SelectCountry
);

const Language = withI18n(({ getMessage, language }) => (
  <div id="language">
    <h2>{getMessage("language_settings")}</h2>
    <p
      dangerouslySetInnerHTML={{
        __html: getMessage("you_speak", {
          language:
            langs.where("1", browserLocale.language).local ||
            "Unknown Language",
          country:
            countries.getName(browserLocale.country, language.language) ||
            countries.getName(browserLocale.country, "en") ||
            "Unknown Country"
        })
      }}
    />
    <p>{getMessage("language_instructions")}</p>
    <p>
      <label>
        {getMessage("language")}
        <br />
        <SelectLanguage />
      </label>
      <br />
      <label>
        {getMessage("country")}
        <br />
        <SelectCountry />
      </label>
    </p>
  </div>
));

const Onboarding = withI18n(({ getMessage, onAcceptClick }) => (
  <div id="tos">
    <div id="main">
      <Language />
      <div
        id="terms"
        dangerouslySetInnerHTML={{ __html: getMessage("terms_of_service") }}
      />
    </div>
    <div id="accept-box">
      <button id="accept" onClick={onAcceptClick}>
        {getMessage("terms_of_service_accept")}
      </button>
    </div>
  </div>
));

let Dispatcher = ({ terms, language, onAcceptClick }) => {
  return (
    <div
      id="popup"
      lang={language.language}
      data-locale={`${language.language}_${language.country}`}
    >
      {terms ? (
        <ContainerWithThanks />
      ) : (
        <Onboarding onAcceptClick={onAcceptClick} />
      )}
    </div>
  );
};

const dispatchToProps = dispatch => ({
  onAcceptClick: e => {
    e.preventDefault();
    dispatch(acceptTerms());
    getAds(store.getState().language, resp => dispatch(newAds(resp)));
  }
});

Dispatcher = connect(state => state, dispatchToProps)(Dispatcher);

render(
  <Provider store={store}>
    <div id="popup">
      <Dispatcher />
    </div>
  </Provider>,
  document.body
);

// connect to the ratings channel
chrome.runtime.onMessage.addListener(ads => store.dispatch(newRatings(ads)));
store.subscribe(() => updateBadge(store.getState().ratings || []));

// Refresh our ads by first filtering out ones the user has seen, and then merging like with
// ratings.
if (store.getState().terms) {
  getAds(store.getState().language, resp => {
    const set = new Set();
    getUnratedRatings(store.getState().ratings).map(rating =>
      set.add(rating.id)
    );
    store.dispatch(newAds(resp.filter(ad => !set.has(ad.id))));
  });
}
