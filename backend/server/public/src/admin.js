import { h, render } from 'preact';
import thunkMiddleware from 'redux-thunk';
import persistState from 'redux-localstorage';
import { createLogger } from 'redux-logger';

const NEW_ADS = "new_ads";
const HIDE_AD = "hide_ad";

const hideAd = (ad) => ({
  type: HIDE_AD,
  value: ad
});

const newAds = (ads) => ({
  type: NEW_ADS,
  value: ads
});

const ads = (state = [], action) => {
  switch(action.type) {
  case NEW_ADS:
    return action.value;
  case HIDE_AD:
    return state.map(ad => {
      if(ad.id === action.id) {
        return { ...ad, suppressed: action.value };
      }
      return ad;
    });
  default:
    return state;
  }
};

const suppressAd = (ad) => {
  return (dispatch) => {
    let body = {
      ...ad,
      suppressed: true
    };
    dispatch(hideAd(ad));
    return fetch("/facebook-ads/suppress", {
      method: "POST",
      body: JSON.stringify(body)
    }).then((resp) => resp.json())
      .then((ads) => dispatch(newAds(ads)));
  };
};
