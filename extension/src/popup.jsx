import { h, render } from 'preact';
import { PropTypes } from 'prop-types';
import { compose, combineReducers, createStore } from 'redux';
import { Provider, connect } from 'preact-redux';
import uniqBy from 'lodash/uniqBy';
import persistState from 'redux-localstorage';

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
const TOGGLE_TAB = "toggle_tab";
const NEW_ADS = "new_ads";
const NEW_RATINGS = "new_ratings";
const ASSIGN_RATING = "assign_rating";

// Actions
const toggle = (value) => ({type: TOGGLE_TAB, value});
const rateAd = (id, rating) => ({
  type: ASSIGN_RATING,
  id: id,
  value: rating
});

const newAds = (ads) => ({
  type: NEW_ADS,
  value: ads
});
const newRatings = (ratings) => ({
  type: NEW_RATINGS,
  value: ratings
});

// Reducers
const active = (state = ToggleType.ADS, action) => {
  switch(action.type) {
  case TOGGLE_TAB:
    return action.value;
  default:
    return state;
  }
};

const uniqueAds = (new_ads, ads) => uniqBy(new_ads.concat(ads), 'id');
const ads = (state = [], action) => {
  switch(action.type) {
  case NEW_ADS:
    return uniqueAds(action.value, state);
  default:
    return state;
  }
};

const ratings = (state = [], action) => {
  switch(action.type) {
  case NEW_RATINGS:
    return uniqueAds(action.value, state);
  case ASSIGN_RATING:
    return state.map(rating => {
      if(rating.id == action.id) {
        return { ...rating, rating: action.value };
      }
      return rating;
    });
  default:
    return state;
  }
};

// The main app!
const reducer = combineReducers({
  active,
  ads,
  ratings
});

const enhancer = compose(persistState());

let store = createStore(reducer, enhancer);

// Utilities
const getTitle = (html) => {
  let node = document.createElement("div");
  node.innerHTML = html;
  return ''; //node.querySelectorAll(".userContent")[0].innerText;
};

// Views
const Ad = ({id, html}) => (
  <div className="ad" id={id}>
    <h2>{getTitle(html)}</h2>
    <div>
      {html}
    </div>
  </div>
);
Ad.propTypes = {
  html: PropTypes.string.isRequired,
  id: PropTypes.string.isRequired
};


// Ads from the server to show
const Ads = ({ads}) => (
  <div id="ads">
    {ads.map(ad => <Ad key={ad.id} {...ad} />)}
  </div>
);
Ads.propTypes = {
  ads: PropTypes.arrayOf(PropTypes.shape(Ad.propTypes)).isRequired
};

// Ads to be rated and sent to the server
const Rating = ({action, id, html}) => (
  <div className="rating" onClick={function(){ return action(id, true); }}>
    <h2>{getTitle(html)}</h2>
    <div className="ad-container">
      {html}
    </div>
  </div>
);
Rating.propTypes = {
  ...Ad.propTypes,
  action: PropTypes.func.isRequired,
  rating: PropTypes.oneOf(Object.values(RatingType)).isRequired,
};

const Ratings = ({onRatingClick, ratings}) => (
  <div id="ratings">
    {ratings.map(rating => (
      <Rating key={rating.id} {...rating} action={onRatingClick} />)
    )}
  </div>
);
Ratings.propTypes = {
  onRatingClick: PropTypes.func.isRequired,
  ratings: PropTypes.arrayOf(PropTypes.shape(Rating.propTypes)).isRequired
};


const getUnratedRatings = (ratings) => (
  ratings.filter(rating => !("rated" in rating))
);

const ratingStateToProps = (state) => ({
  ratings: getUnratedRatings(state.ratings)
});

const ratingDispatchToProps = (dispatch) => ({
  onRatingClick: (id, rating) => {
    dispatch(rateAd(id, rating));
  }
});

const UnratedRatings = connect(
  ratingStateToProps,
  ratingDispatchToProps
)(Ratings);

// Controls which section of tabs to show, defaults to the
const Toggle = ({type, message, active_tab, onToggleClick}) => (
  <span
    className={'toggle' + (active_tab == type ? ' active' : '')}
    id="rate-toggle"
    onClick={function() { onToggleClick(type); }}
  >
    {getMessage(message)}
  </span>
);
const TogglePropType = PropTypes.oneOf(Object.values(ToggleType)).isRequired;
Toggle.propTypes = {
  active_tab: TogglePropType,
  message: PropTypes.string.isRequired,
  onToggleClick: PropTypes.func.isRequired,
  type: TogglePropType
};

// Our Main container.
let Toggler = ({ads, ratings, active_tab, onToggleClick}) => (
  <div id="toggler">
    <div id="toggles">
      <Toggle
        active_tab={active_tab}
        message="see_ads" onToggleClick={onToggleClick}
        type={ToggleType.ADS}
      />
      <Toggle
        active_tab={active_tab}
        message="rate_ads" onToggleClick={onToggleClick}
        type={ToggleType.RATER}
      />
    </div>
    <div id="container">
      {active_tab == ToggleType.ADS ?
        <Ads ads={ads} /> :
        <UnratedRatings ratings={ratings} />}
    </div>
  </div>
);
Toggler.propTypes = {
  active_tab: Toggle.propTypes.active_tab,
  ads: Ads.propTypes.ads,
  onToggleClick: Toggle.propTypes.onToggleClick,
  ratings: Ratings.propTypes.ratings
};

const togglerDispatchToProps = (dispatch) => ({
  onToggleClick: (type) => {
    dispatch(toggle(type));
  }
});

Toggler = connect(
  (state) => state,
  togglerDispatchToProps
)(Toggler);

render(
  <Provider store={store}>
    <div id="popup">
      <Toggler />
    </div>
  </Provider>,
  document.body
);

// connect to the ratings channel
chrome.runtime.onMessage.addListener((ads) => store.dispatch(newRatings(ads)));
