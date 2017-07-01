import { h, render } from 'preact';
import { PropTypes } from 'proptypes';
import { combineReducers, createStore, connect } from 'redux';
import { Provider } from 'preact-redux';

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
const rateAd = () => ({type: ASSIGN_RATING, value: RatingType.POLITICAL});


// Reducers
const active = (state = ToggleType.ADS, action) => {
  switch(action.type) {
  case TOGGLE_TAB:
    return action.value;
  default:
    return state;
  }
};

const ads = (state = [], action) => {
  return state;
};

const ratings = (state = [], action) => {
  return state;
};

// The main app!
const reducer = combineReducers({
  active,
  ads,
  ratings
});

let store = createStore(reducer);

// Views
const Ad = ({id, html}) => (
  <div className="ad" />
);

Ad.propTypes = {
  id: PropTypes.string.isRequired,
  html: PropTypes.string.isRequired
};

// Ads from the server to show
const Ads = ({ads}) => (
  <div id="ads">
    {ads.map(ad => <Ad key={ad.id} {...ad} />)}
  </div>
);

Ads.propTypes = {
  ads: PropTypes.arrayOf(PropTypes.shape(Ad.propTypes))
};

// Ads to be rated and sent to the server
const Rating = ({action, rating}) => (
  <div className="rating" onClick={() => action(rating.id, true)}/>
);

Rating.proptypes = Object.assign({}, Ad.propTypes, {
  rating: PropTypes.oneOf(Object.values(RatingType)).isRequired,
  action: PropTypes.func.isRequired
});

const Ratings = ({onRatingClick, ratings}) => (
  <div id="ratings">
    {ratings.map(rating => (
      <Rating key={rating.id} {...rating} action={onRatingClick} />)
    )}
  </div>
);

Ratings.propTypes = {
  ratings: PropTypes.arrayOf(PropTypes.shape(Rating.propTypes)).isRequired,
  onRatingClick: PropTypes.func.isRequired
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
  <span className={'toggle' + (active_tab == type ? ' active' : '')}
    onClick={() => onToggleClick(type)}
    id="rate-toggle">
    {getMessage(message)}
  </span>
);

const TogglePropType = PropTypes.oneOf(Object.values(ToggleType)).isRequired;
Toggle.propTypes = {
  type: TogglePropType,
  message: PropTypes.string.isRequired,
  active_tab: TogglePropType,
  onToggleClick: PropTypes.func.isRequired
};

// Our Main container.
let Toggler = ({ads, ratings, active_tab, onToggleClick}) => (
  <div id="toggler">
    <div id="toggles">
      <Toggle type={ToggleType.ADS}
        active_tab={active_tab}
        message="see_ads" onToggleClick={onToggleClick} />
      <Toggle type={ToggleType.RATER}
        active_tab={active_tab}
        message="rate_ads" onToggleClick={onToggleClick} />
    </div>
    <div id="ads">
      {active_tab == ToggleType.ADS ?
        <Ads ads={ads} /> :
        <UnratedRatings ratings={ratings} />}
    </div>
  </div>
);

Toggler.propTypes = {
  ads: Ads.propTypes.ads,
  ratings: Ratings.propTypes.ratings,
  active_tab: Toggle.propTypes.active_tab,
  onToggleClick: Toggle.propTypes.onToggleClick
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
