import { h, render } from 'preact';
import { applyMiddleware, compose, combineReducers, createStore } from 'redux';
import thunkMiddleware from 'redux-thunk';
import persistState from 'redux-localstorage';
import { createLogger } from 'redux-logger';

const NEW_ADS = "new_ads";
const HIDE_AD = "hide_ad";
const LOGIN = "login";

const b64 = (thing) => btoa(thing).replace('+', '-').replace('_', '/').replace(/=+/, '');
const createJWT = (username, password) => {
  const encoder = new TextEncoder();
  const header = {
    alg: "HS256",
    typ: "JWT"
  };
  const payload = {
    username
  };
  const base = `${b64(JSON.stringify(header))}.${b64(JSON.stringify(payload))}`;
  const encoded = encoder.encode(base);
  return crypto.subtle.importKey(
    "raw",
    encoder.encode(password),
    {name: "HMAC", hash: {name: "SHA-256"}},
    false,
    ["sign"]
  ).then(key => crypto.subtle.sign({name: 'HMAC'}, key, encoded))
    .then(signature => `${base}.${b64(String.fromCharCode.apply(null, new Uint8Array(signature)))}`);
};

const hideAd = (ad) => ({
  type: HIDE_AD,
  value: ad
});

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

const newAds = (ads) => ({
  type: NEW_ADS,
  value: ads
});

const login = (credentials) => ({
  type: LOGIN,
  value: credentials
});

const authorize = (username, password) => {
  // create jwt
  return (dispatch) => createJWT(username, password).then(token =>
    fetch("/facebook-ads/login", {
      method: "POST",
      headers: new Headers({
        "Authorization": `Bearer ${token}`
      })
    }).then(() => dispatch(login(token))));
};

const credentials = (state = {}, action) => {
  switch(action.type) {
  case LOGIN:
    return action.value;
  default:
    return state;
  }
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

const reducer = combineReducers({
  credentials,
  ads,
});
const middleware = [thunkMiddleware, createLogger()];
const store = createStore(reducer, compose(...[persistState(), applyMiddleware(...middleware)]));
