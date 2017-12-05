import React from 'react';
import ReactDOM from 'react-dom';
import { applyMiddleware, compose, combineReducers, createStore } from 'redux';
import thunkMiddleware from 'redux-thunk';
import persistState from 'redux-localstorage';
import { Provider, connect } from 'react-redux';
import { createLogger } from 'redux-logger';
import { headers, NEW_ADS, refresh, newSearch, search, enableBatching, deserialize } from 'utils.js';
import { entities, targets, advertisers, filters } from 'filters.jsx';
import { Pagination, pagination } from 'pagination.jsx';


import { go } from 'i18n.js';

import { debounce } from "lodash";

const HIDE_AD = "hide_ad";
const LOGIN = "login";
const LOGOUT = "logout";

const b64 = (thing) => btoa(thing).replace(/\+/g, '-').replace(/\//g, '_').replace(/=/g, '');
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
  return window.crypto.subtle.importKey(
    "raw",
    encoder.encode(password),
    {name: "HMAC", hash: {name: "SHA-256"}},
    false,
    ["sign"]
  ).then(key => window.crypto.subtle.sign({name: 'HMAC'}, key, encoded))
    .then(signature => ({token: `${base}.${b64(String.fromCharCode.apply(null, new Uint8Array(signature)))}`}));
};

const hideAd = (ad) => ({
  type: HIDE_AD,
  id: ad.id
});

const suppressAd = (ad) => {
  return (dispatch, getState) => {
    dispatch(hideAd(ad));
    return fetch("/facebook-ads/admin/ads", {
      method: "POST",
      body: ad.id,
      headers: headers(getState().credentials)
    }).then((resp) => {
      if(resp.ok) {
        console.log("suppressed");
      } else {
        dispatch(logout());
      }
    });
  };
};

const login = (credentials) => ({
  type: LOGIN,
  value: credentials
});

const logout = () => ({
  type: LOGOUT
});

const authorize = (username, password) => {
  // create jwt
  return (dispatch) => createJWT(username, password).then(token => {
    return fetch("/facebook-ads/login", {
      method: "POST",
      headers: headers(token)
    }).then((resp) => {
      if(resp.ok) {
        dispatch(login(token));
      }
    });
  });
};

const credentials = (state = {}, action) => {
  switch(action.type) {
  case LOGIN:
    return action.value;
  case LOGOUT:
    return {};
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
        return { ...ad, suppressed: true };
      }
      return ad;
    });
  default:
    return state;
  }
};

const reducer = enableBatching(combineReducers({
  ads,
  search,
  entities,
  advertisers,
  targets,
  filters,
  pagination,
  credentials
}));

const middleware = [thunkMiddleware, createLogger()];
const store = createStore(reducer, compose(...[persistState(), applyMiddleware(...middleware)]));

const Ad = ({ad, onClick}) => (
  <div className="ad">
    <table>
      <tr>
        <td>id</td>
        <td>{ad.id}</td>
      </tr>
      <tr>
        <td>first seen</td>
        <td>{new Date(Date.parse(ad.created_at)).toString()}</td>
      </tr>
      <tr>
        <td>title</td>
        <td>{ad.title}</td>
      </tr>
      <tr>
        <td>text</td>
        <td dangerouslySetInnerHTML={{__html: ad.html}} />
      </tr>
      <tr>
        <td>targeting</td>
        <td dangerouslySetInnerHTML={{__html: ad.targeting}} />
      </tr>
      <tr>
        <td>
          political / not political
        </td>
        <td>
          {ad.political} / {ad.not_political}
        </td>
      </tr>
      <tr>
        <td colSpan="2">
          {ad.suppressed ? "Suppressed" : <button onClick={function() { return onClick(ad); }}>
              Suppress
          </button>}
        </td>
      </tr>
    </table>
  </div>
);

let Ads = ({ads, onClick, onKeyUp}) => (
  <div id="ads">
    <input id="search" placeholder="Search for ads" onKeyUp={onKeyUp} />
    <Pagination />
    {ads.map((ad) => <Ad ad={ad} key={ad.id} onClick={onClick} />)}
  </div>
);
Ads = connect(
  (state) => ({
    ads: state.ads.filter((ad) => !ad.suppressed)
  }),
  (dispatch) => ({
    onClick: (ad) => dispatch(suppressAd(ad)),
    onKeyUp: debounce((e) => {
      e.preventDefault();
      dispatch(newSearch(e.target.value.length ? e.target.value : null));
    }, 1000)
  })
)(Ads);

let Login = ({ dispatch }) => {
  let email, password;
  const onLogin = (e) => {
    e.preventDefault();
    dispatch(authorize(email.value, password.value));
  };
  return <form id="login" onSubmit={onLogin} >
    <input id="email" type="text" ref={(node) => email = node } placeholder="email" />
    <input id="password" type="password" ref={(node) => password = node } placeholder="password" />
    <input id="submit" type="submit" value="login" />
  </form>;
};
Login = connect()(Login);

let App = ({credentials}) => (
  <div id="app">
    {credentials && credentials.token ? <Ads /> : <Login />}
  </div>
);
App = connect((state) => state)(App);

go(() => {
  ReactDOM.render(
    <Provider store={store}>
      <App />
    </Provider>,
    document.body
  );
  deserialize(store.dispatch);
  refresh(store).then(() => store.subscribe(() => refresh(store)));
});
