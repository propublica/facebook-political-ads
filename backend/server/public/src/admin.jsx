import React from "react";
import ReactDOM from "react-dom";
import { applyMiddleware, compose, combineReducers, createStore } from "redux";
import thunkMiddleware from "redux-thunk";
import persistState from "redux-localstorage";
import { Provider, connect } from "react-redux";
import { createLogger } from "redux-logger";
import {
  headers,
  NEW_ADS,
  refresh,
  newSearch,
  search,
  enableBatching,
  deserialize,
  lang,
  GOT_THAT_AD,
  REQUESTING_ONE_AD,
  getOneAd
} from "utils.js";
import { entities, targets, advertisers, filters } from "filters.jsx";
import { Pagination, pagination } from "pagination.jsx";

import { go } from "i18n.js";

import { debounce } from "lodash";

const HIDE_AD = "hide_ad";
const LOGIN = "login";
const LOGOUT = "logout";

const b64 = thing =>
  btoa(thing)
    .replace(/\+/g, "-")
    .replace(/\//g, "_")
    .replace(/=/g, "");
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
  return window.crypto.subtle
    .importKey(
      "raw",
      encoder.encode(password),
      { name: "HMAC", hash: { name: "SHA-256" } },
      false,
      ["sign"]
    )
    .then(key => window.crypto.subtle.sign({ name: "HMAC" }, key, encoded))
    .then(signature => ({
      token: `${base}.${b64(
        String.fromCharCode.apply(null, new Uint8Array(signature))
      )}`
    }));
};

const hideAd = ad => ({
  type: HIDE_AD,
  id: ad.id
});

const suppressAd = ad => {
  return (dispatch, getState) => {
    dispatch(hideAd(ad));
    return fetch("/facebook-ads/admin/ads", {
      method: "POST",
      body: ad.id,
      headers: headers(getState().credentials)
    }).then(resp => {
      if (resp.ok) {
        console.log("suppressed");
      } else {
        dispatch(logout());
      }
    });
  };
};

const login = credentials => ({
  type: LOGIN,
  value: credentials
});

const logout = () => ({
  type: LOGOUT
});

const authorize = (username, password) => {
  // create jwt
  return dispatch =>
    createJWT(username, password).then(token => {
      return fetch("/facebook-ads/login", {
        method: "POST",
        headers: headers(token)
      }).then(resp => {
        if (resp.ok) {
          dispatch(login(token));
        }
      });
    });
};

const credentials = (state = {}, action) => {
  switch (action.type) {
    case LOGIN:
      return action.value;
    case LOGOUT:
      return {};
    default:
      return state;
  }
};

// this is a reducer.
const ads = (state = [], action) => {
  switch (action.type) {
    case NEW_ADS:
      return action.value;
    case HIDE_AD:
      return state.map(ad => {
        if (ad.id === action.id) {
          return { ...ad, suppressed: true };
        }
        return ad;
      });
    default:
      return state;
  }
};

const permalinked_ad = (state = {}, action) => {
  let new_ad_obj = {};
  switch (action.type) {
    case GOT_THAT_AD:
      new_ad_obj[action.ad.id] = Object.assign({}, action.ad, { loaded: true });
      return Object.assign({}, state, {
        requested_ad_id: action.ad.id,
        ads: Object.assign({}, state.ads, new_ad_obj)
      });
    case REQUESTING_ONE_AD:
      new_ad_obj[action.ad_id] = { loaded: false };
      return Object.assign({}, state, {
        requested_ad_id: action.ad_id,
        ads: Object.assign({}, state.ads, new_ad_obj)
      });
    default:
      return state;
  }
};

const reducer = enableBatching(
  combineReducers({
    ads,
    permalinked_ad,
    search,
    entities,
    advertisers,
    targets,
    filters,
    pagination,
    credentials,
    lang
  })
);

const middleware = [thunkMiddleware, createLogger()];
const store = createStore(
  reducer,
  compose(...[persistState("credentials"), applyMiddleware(...middleware)])
);

const Ad = ({ ad, onSuppressClick, onPermalinkClick }) => (
  <div className="ad">
    <table>
      <tbody>
        <tr>
          <td>id</td>
          <td>
            <a
              href={"?detail=" + ad.id}
              onClick={e => {
                e.preventDefault();
                onPermalinkClick(ad.id);
              }}
            >
              {ad.id}
            </a>
          </td>
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
          <td dangerouslySetInnerHTML={{ __html: ad.html }} />
        </tr>
        <tr>
          <td>targeting</td>
          <td dangerouslySetInnerHTML={{ __html: ad.targeting }} />
        </tr>
        <tr>
          <td>political / not political</td>
          <td>
            {ad.political} / {ad.not_political}
          </td>
        </tr>
        <tr>
          <td>likelihood</td>
          <td>{ad.political_probability}</td>
        </tr>
        <tr>
          <td>impressions</td>
          <td>{ad.impressions}</td>
        </tr>
        <tr>
          <td colSpan="2">
            {ad.suppressed ? (
              "Suppressed"
            ) : (
              <button
                onClick={function() {
                  return onSuppressClick(ad);
                }}
              >
                Suppress
              </button>
            )}
          </td>
        </tr>
      </tbody>
    </table>
  </div>
);

let Ads = ({
  ads,
  onSuppressClick,
  onKeyUp,
  onPermalinkClick,
  search,
  pagination
}) => (
  <div id="ads">
    <input
      id="search"
      placeholder="Search for ads"
      onKeyUp={onKeyUp}
      search={search}
    />
    {pagination ? <Pagination /> : ""}
    {ads.map(ad => (
      <Ad
        ad={ad}
        key={ad.id}
        onSuppressClick={onSuppressClick}
        onPermalinkClick={onPermalinkClick}
      />
    ))}
  </div>
);
const throttledDispatch = debounce((dispatch, input) => {
  dispatch(newSearch(input));
}, 750);
Ads = connect(
  ({ ads, search }) => ({
    ads: ads.filter(ad => !ad.suppressed),
    credentials, // these are needed for eventually creating links
    lang, // these are needed for eventually creating links
    search
  }),
  dispatch => ({
    onSuppressClick: ad => dispatch(suppressAd(ad)),
    onKeyUp: e => {
      e.preventDefault();
      throttledDispatch(
        dispatch,
        e.target.value.length ? e.target.value : null
      );
    },
    onPermalinkClick: ad_id => {
      dispatch(getOneAd(ad_id));
    }
  })
)(Ads);

let Login = ({ dispatch }) => {
  let email, password;
  const onLogin = e => {
    e.preventDefault();
    dispatch(authorize(email.value, password.value));
  };
  return (
    <form id="login" onSubmit={onLogin}>
      <input
        id="email"
        type="text"
        ref={node => (email = node)}
        placeholder="email"
      />
      <input
        id="password"
        type="password"
        ref={node => (password = node)}
        placeholder="password"
      />
      <input id="submit" type="submit" value="login" />
    </form>
  );
};
Login = connect()(Login);

let App = ({ credentials }) => {
  return (
    <div id="app">
      <h1>
        <a href="/facebook-ads/admin?">FBPAC Admin</a>
      </h1>
      {credentials && credentials.token ? <Ads /> : <Login />}
    </div>
  );
};
App = connect(state => state)(App);

go(() => {
  ReactDOM.render(
    <Provider store={store}>
      <App />
    </Provider>,
    document.querySelector("#react-root")
  );

  deserialize(store.dispatch);
  // I'll figure out a better way to do this but I'm learning React so 🐻 with me.
  // we're probably going to want a router? maybe?
  const searchParams = new URLSearchParams(location.search);
  if (searchParams.get("detail")) {
    store.dispatch(getOneAd(searchParams.get("detail")));
  }
  // anytime anything changes, then make the ajax request whenever the user changes the facets they
  // want.
  refresh(store).then(() => store.subscribe(() => refresh(store)));
});
