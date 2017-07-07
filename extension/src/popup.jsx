import { h, render } from 'preact';
import { PropTypes } from 'prop-types';
import { compose, combineReducers, createStore } from 'redux';
import { Provider, connect } from 'preact-redux';
import uniqBy from 'lodash/uniqBy';
import persistState from 'redux-localstorage';
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

// Ad utilities

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

const getContent = (html) => {
  let p = query(html, '.userContent p');
  if(p)
    return p.innerHTML;
};

const getAdvertiser = (html) => {
  let a = query(html, 'h5 a') || query(html, 'h6 a');
  if(a)
    return a.innerText;
};

const insertAdFields = (ads) => (
  ads.map((ad) => ({
    ...ad,
    image: getImage(ad.html),
    content: getContent(ad.html),
    advertiser: getAdvertiser(ad.html)
  }))
);

// Views
const Ad = ({advertiser, bigImage, content, id, image}) => (
  <div className="ad" id={id}>
    <div className="chiclet">
      {image ? <img src={image} /> : ''}
    </div>
    <div className="ad-display">
      <div className="advertiser">{advertiser}</div>
      <div className="ad-content" dangerouslySetInnerHTML={{__html:content}} />
    </div>
    {bigImage ? <img className="" src={bigImage} /> : ''}
  </div>
);
Ad.defaultProps = {
  bigImage: null
};
Ad.propTypes = {
  advertiser: PropTypes.string.isRequired,
  bigImage: PropTypes.string,
  content: PropTypes.string.isRequired,
  id: PropTypes.string.isRequired,
  image: PropTypes.string.isRequired
};


// Ads from the server to show
let Ads = ({ads}) => (
  <div id="ads">
    {ads.map(ad => <Ad key={ad.id} {...ad} />)}
  </div>
);
Ads = connect((state) => ({
  ads: insertAdFields(state.ads)
}))(Ads);
Ads.propTypes = {
  ads: PropTypes.arrayOf(PropTypes.shape(Ad.propTypes)).isRequired
};


// Ads to be rated and sent to the server
const Rating = ({action, advertiser, id, image, content, rating = RatingType.NORMAL}) => (
  <div className="rating" onClick={function(){ return action(id, rating); }}>
    <Ad advertiser={advertiser} content={content} id={id} image={image} />
    <div className="rater">
      <input
        id={'political_' + id}
        onClick={function(){ return action(id, RatingType.POLITICAL); }}
        type="radio"
      />
      <input
        onClick={function(){ return action(id, RatingType.POLITICAL); }}
        text="Normal"
        type="radio"
      />
    </div>
  </div>
);
Rating.defaultProps = {
  rating: RatingType.NORMAL
};
Rating.propTypes = {
  ...Ad.propTypes,
  action: PropTypes.func.isRequired,
  rating: PropTypes.oneOf(Object.values(RatingType))
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

const ratingsStateToProps = (state) => ({
  ratings: insertAdFields(getUnratedRatings(state.ratings))
});

const ratingsDispatchToProps = (dispatch) => ({
  onRatingClick: (id, rating) => {
    dispatch(rateAd(id, rating));
  }
});

const UnratedRatings = connect(
  ratingsStateToProps,
  ratingsDispatchToProps
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
const TogglePropType = PropTypes.oneOf(Object.values(ToggleType));
Toggle.propTypes = {
  active_tab: TogglePropType.isRequired,
  message: PropTypes.string.isRequired,
  onToggleClick: PropTypes.func.isRequired,
  type: TogglePropType.isRequired
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
  null,
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
