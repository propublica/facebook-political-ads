import { h, render } from 'preact';
import thunkMiddleware from 'redux-thunk';
import { applyMiddleware, compose, combineReducers, createStore } from 'redux';
import { Provider, connect } from 'preact-redux';
import persistState from 'redux-localstorage';
import { createLogger } from 'redux-logger';
import { sendAds, getAds, updateBadge, adForRequest } from 'utils.js';
// styles
import "../css/styles.css";

const getMessage = chrome.i18n.getMessage;

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

// Actions
const acceptTerms = () => ({ type: ACCEPT_TERMS });
const toggle = (value) => ({ type: TOGGLE_TAB, value });
const newAds = (ads) => ({
  type: NEW_ADS,
  value: ads
});
const newRatings = (ratings) => ({
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
const rateAd = (ad, rating, update) => {
  return (dispatch) => {
    let body = {
      ...adForRequest(ad),
      political: rating === RatingType.POLITICAL,
    };
    dispatch(update(ad.id, rating));
    let cb = () => ({});
    return sendAds([body]).then(cb, cb);
  };
};

// Reducers
const active = (state = ToggleType.RATER, action) => {
  switch(action.type) {
  case TOGGLE_TAB:
    return action.value;
  default:
    return state;
  }
};

const mergeAds = (ads, newAds) => {
  let ids = new Map(ads.map(ad => [ad.id, ad]));
  newAds.forEach(ad => {
    if(ids.has(ad.id)) {
      let old = ids.get(ad.id);
      ids.delete(ad.id);
      let newAd = Object.assign({}, old, ad);
      ids.set(newAd.id, newAd);
    } else {
      ids.set(ad.id, ad);
    }
  });
  return Array.from(ids.values())
    .sort((a, b) => a.id > b.id ? 1 : -1)
    .sort((a) => a.rating === RatingType.POLITICAL ? 1 : -1);
};

const buildUpdate = (type) => ((state = [], action) => {
  switch(action.type) {
  case "new_" + type + "s":
    return mergeAds(state, action.value);
  case "update_" + type:
    return state.map(ad => {
      if(ad.id === action.id) {
        return { ...ad, rating: action.value };
      }
      return ad;
    });
  default:
    return state;
  }
});

const terms = (state = false, action) => {
  switch(action.type) {
  case ACCEPT_TERMS:
    return true;
  default:
    return state;
  }
};

// The main reducer!
const reducer = combineReducers({
  active,
  ads: buildUpdate("ad"),
  ratings: buildUpdate("rating"),
  terms
});

let middleware = [thunkMiddleware];
if(process.env.NODE_ENV === 'development') {
  middleware.push(createLogger());
}
const enhancer = compose(...[persistState(), applyMiddleware(...middleware)]);
let store = createStore(reducer, enhancer);

// Ad utilities
const getUnratedRatings = (ratings) => (
  ratings.filter(rating => rating.rating === RatingType.POLITICAL || !("rating" in rating))
);

let div = document.createElement('div');
const query = (html, selector) => {
  div.innerHTML = html;
  return div.querySelector(selector);
};

const getImage = (html) => {
  let img = query(html, 'img');
  if(img)
    return img.getAttribute('src');
};

const getAdMessage = (html) => {
  let p = query(html, '.userContent p') || query(html, 'span');
  if(p)
    return p.innerHTML;
};

const getTitle = (html) => {
  let a = query(html, 'h5 a') || query(html, 'h6 a') || query(html, 'strong');
  if(a)
    return a.innerText;
};

const insertAdFields = (ads) => (
  ads.map((ad) => ({
    ...ad,
    image: getImage(ad.html),
    message: getAdMessage(ad.html),
    title: getTitle(ad.html)
  }))
);

// Views
const Ad = ({title, message, id, image}) => (
  <div className="ad" id={id}>
    <div className="chiclet">
      {image ? <img src={image} /> : ''}
    </div>
    <div className="ad-display">
      <div className="advertiser">{title}</div>
      <div className="ad-content" dangerouslySetInnerHTML={{__html:message}} />
    </div>
  </div>
);

const RatingForm = ({rating, action, question})=> (
  <div className="rater">
    {getMessage(question)}
    <button
      id={'political' + rating.id}
      onClick={function(){ return action(rating, RatingType.POLITICAL); }}
    >
      {getMessage('political')}
    </button>
    <button
      id={'normal' + rating.id}
      onClick={function(){ return action(rating, RatingType.NORMAL); }}
    >
      {getMessage('normal')}
    </button>
  </div>
);

// Ads to be rated and sent to the server
const Rating = ({rating, action, question}) => (
  <div className="rating">
    <Ad
      title={rating.title}
      message={rating.message}
      id={rating.id}
      image={rating.image}
    />
    {("rating" in rating) ?
      <b className="political">{getMessage('political')}</b> :
      <RatingForm action={action} rating={rating} question={question} /> }
  </div>
);

const Ratings = ({onRatingClick, ratings}) => (
  <div id="ratings">
    {ratings.map(rating =>
      (<Rating key={rating.id} rating={rating} action={onRatingClick} question="rating_question" />)
    )}
  </div>
);
const ratingsStateToProps = (state) => ({
  ratings: insertAdFields(getUnratedRatings(state.ratings))
});
const ratingsDispatchToProps = (dispatch) => ({
  onRatingClick: (id, rating) => {
    dispatch(rateAd(id, rating, updateRating));
  }
});
const UnratedRatings = connect(
  ratingsStateToProps,
  ratingsDispatchToProps
)(Ratings);

// Ads from the server to show
let Ads = ({ads, onAdClick}) => (
  <div id="ads">
    {ads.map(ad =>
      (<Rating key={ad.id} rating={ad} action={onAdClick} question="verify_question" />)
    )}
  </div>
);
const adStateToProps = (state) => ({
  ads: getUnratedRatings(state.ads)
});
const adDispatchToProps = (dispatch) => ({
  onAdClick: (id, rating) => {
    dispatch(rateAd(id, rating, updateAd));
  }
});
Ads = connect(
  adStateToProps,
  adDispatchToProps
)(Ads);

// Controls which section of tabs to show, defaults to the
const Toggle = ({type, message, active, onToggleClick}) => (
  <div
    className={'toggle' + (active === type ? ' active' : '')}
    onClick={function() { onToggleClick(type); }}
  >
    {getMessage(message)}
  </div>
);

// Our Main container.
let Toggler = ({ads, ratings, active, onToggleClick}) => (
  <div id="toggler">
    <div id="tabs">
      <Toggle
        active={active}
        message="rate_ads" onToggleClick={onToggleClick}
        type={ToggleType.RATER}
      />
      <Toggle
        active={active}
        message="see_ads" onToggleClick={onToggleClick}
        type={ToggleType.ADS}
      />
    </div>
    <div id="container">
      {active === ToggleType.ADS ?
        <Ads ads={ads} /> :
        <UnratedRatings ratings={ratings} />}
    </div>
  </div>
);

const togglerDispatchToProps = (dispatch) => ({
  onToggleClick: (type) => {
    dispatch(toggle(type));
  }
});

Toggler = connect(
  (state) => (state),
  togglerDispatchToProps
)(Toggler);

const Onboarding = ({onAcceptClick}) => (
  <div id="tos">
    <div id="terms" dangerouslySetInnerHTML={{__html:getMessage("terms_of_service")}} />
    <div id="accept-box">
      <button id="accept" onClick={function(){ return onAcceptClick(); }}>
        Accept
      </button>
    </div>
  </div>
);

let Dispatcher = ({terms, onAcceptClick}) => {
  if(terms) {
    return <Toggler />;
  } else {
    return <Onboarding onAcceptClick={onAcceptClick}/>;
  }
};

const dispatchToProps = (dispatch) => ({
  onAcceptClick: () => {
    dispatch(acceptTerms());
  }
});

Dispatcher = connect(
  (state) => ({terms: state.terms}),
  dispatchToProps
)(Dispatcher);

render(
  <Provider store={store}>
    <div id="popup">
      <Dispatcher />
    </div>
  </Provider>,
  document.body
);

// connect to the ratings channel
chrome.runtime.onMessage.addListener((ads) => store.dispatch(newRatings(ads)));
store.subscribe(() => updateBadge(store.getState().ratings || []));
// Refresh our ads filtering out ones the user has seen.
getAds((resp) => {
  const set = new Set();
  getUnratedRatings(store.getState().ratings).map((rating) => set.add(rating.id));
  newAds(resp.filter((ad) => set.has(ad.id)));
});
