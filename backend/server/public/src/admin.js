import { h, render } from 'preact';
import { applyMiddleware, compose, combineReducers, createStore } from 'redux';
import thunkMiddleware from 'redux-thunk';
import persistState from 'redux-localstorage';
import { createLogger } from 'redux-logger';

const NEW_ADS = "new_ads";
const HIDE_AD = "hide_ad";
const LOGIN = "login";

const hideAd = (ad) => ({
  type: HIDE_AD,
  value: ad
});

const newAds = (ads) => ({
  type: NEW_ADS,
  value: ads
});

const login = (credentials) => ({
  type: LOGIN,
  value: credentials
});

const credentials = (state = {}, action) => {
  switch(action.type) {
  case LOGIN:
    return action.value;
  default:
    return state;
  }
};

const createJWT = (username, password) = {
  const header = {
    alg: "HS256",
    "typ": "JWT"
  };
  const payload = {
    username, password
  };

  const token = `${btoa(JSON.stringify(header))}.${btoa(JSON.stringify(payload))}`;
  cosnt buf = new TextEncoder("utf-8").encode(token);
  return `${token}.${}`
};

const login = (username, password) => {
  // create jwt
  return (dispatch) => {
    const token = createJWT(username, password);
    return fetch("/facebook-ads/login", {
      method: "POST",
      headers: new Headers({
        "Authorization": `Bearer ${token}`
      }).then(() => dispatch(login(token)))
    });
  };
};

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
  return (dispatch, getState) => {
    let body = {
      ...ad,
      suppressed: true
    };
    dispatch(hideAd(ad));

    return fetch("/facebook-ads/suppress", {
      method: "POST",
      body: JSON.stringify(body),
      headers: new Headers({
        "Authorization": "Bearer " + getState().credentials
      })
    }).then((resp) => resp.json())
      .then((ads) => dispatch(newAds(ads)));
  };
};

const reducer = combineReducers({
  credentials,
  login
});
const middleware = [thunkMiddleware, createLogger()];
const store = createStore(reducer, compose(...[persistState(), applyMiddleware(...middleware)]));
