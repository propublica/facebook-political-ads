import { h, render } from 'preact';
import { PropTypes } from 'proptypes';
import { combineReducers, createStore } from 'redux';

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
const toggle = (value) => {
  return {type: TOGGLE_TAB, value};
};

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
const app = combineReducers({
  active,
  ads,
  ratings
});

let store = createStore(app);

// Views
const Ad = ({ad}) => {
  return <div class="ad" />;
};

const Ads = ({ads}) => {
  return <div id="ads">
    {ads.map((ad) => <Ad ad=ad />)}
  </div>;
};

const Rating = ({rating}) => {
  return <div class="rating" />;
};

const Ratings = ({ratings}) => {
  return <div id="ratings">
    {ratings.map((rating) => <Rating rating=rating />)}
  </div>
};

const Toggler = ({active_tab, ads, ratings}) => {
  return <div id="toggler">
    <div id="toggles">
      <span class="toggle {active_tab == ToggleType.ADS}" id="rate-toggle">
        {getMessage("rate_ads")}
      </span>
      <span class="toggle {active_tab == ToggleType.RATER}" id="seeads-toggle">
        {getMessage("see_ads")}
      </span>
    </div>
    <div id="ads">
      {active_tab == ToggleType.ADS ? <Ads ads=ads /> : <Ratings ratings=ratings />}
    </div>
  </div>
};

render(
  <div id="popup">
    <Toggler />
  </div>,
document.body);
