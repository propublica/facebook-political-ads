import React from "react";
import ReactDOM from "react-dom";
import { BrowserRouter, withRouter, Route, Link } from 'react-router-dom';
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

const Ad = ({ ad, onSuppressClick }) => (
  <div className="ad">
    <table>
      <tbody>
        <tr>
          <td>id</td>
          <td>
            <Link to={`/facebook-ads/admin/ads/${ad.id}`}>{ad.id}</Link>
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
                onClick={function () {
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

class AdDetail extends React.Component {
  constructor(props) {
    super(props);
  }

  componentDidMount() {
    console.log(this.props);
    let ad_id = null;
    if (this.props.match) {
      ad_id = this.props.match.params.ad_id;
    } else {
      ad_id = this.props.requested_ad_id; // really state.permalinked_ad.requested_ad_id
    }
    store.dispatch(getOneAd(ad_id));
  }

  render() {
    if (this.props.ads && this.props.ads[this.props.requested_ad_id].loaded) {
      if (this.props.ads[this.props.requested_ad_id].id) {
        return (<div id="ad">
          <input id="search" placeholder="Search for ads" />

          <Ad ad={this.props.ads[this.props.requested_ad_id]} onSuppressClick={this.props.onSuppressClick} />

        </div>);
      } else {
        return (<div><h2>Uh oh, an ad with that ID couldn&apos;t be found!</h2></div>);
      }
    } else {
      return (<div><h2>Loading...</h2></div>);
    }
  }
}
AdDetail = withRouter(connect(
  ({ permalinked_ad }) => (
    { // this is a mapStateToProps function. { ads } is destructuring the `store` hash and getting the `ads` element.
      ads: permalinked_ad.ads,
      requested_ad_id: permalinked_ad.requested_ad_id,

    }),
  (dispatch) => ({ // ownProps is available as a second argument here.
    onSuppressClick: (ad) => dispatch(suppressAd(ad))
  })
)(AdDetail));

class Ads extends React.Component {
  constructor(props) {
    super(props);
  }

  componentDidMount() {
    deserialize(store.dispatch);
    refresh(store).then(() => store.subscribe(() => refresh(store))); // anytime anything changes, then make the ajax request whenever the user changes the facets they want.
  }

  render() {
    return (<div id="ads">
      <input
        id="search"
        placeholder="Search for ads"
        onKeyUp={this.props.onKeyUp}
        search={this.props.search}
      />
      {this.props.pagination ? <Pagination /> : ""}
      {this.props.ads.map(ad => (
        <Ad
          ad={ad}
          key={ad.id}
          onSuppressClick={this.props.onSuppressClick}
        />
      ))}
    </div>);
  }
}

const throttledDispatch = debounce((dispatch, input) => {
  dispatch(newSearch(input));
}, 750);

Ads = withRouter(connect(
  ({ ads, search, page }) => ({
    ads: ads.filter(ad => !ad.suppressed),
    credentials, // these are needed for eventually creating links
    lang,        // these are needed for eventually creating links
    search,
    pagination,
    page
  }),
  dispatch => ({
    onSuppressClick: ad => dispatch(suppressAd(ad)),
    onKeyUp: e => {
      e.preventDefault();
      throttledDispatch(
        dispatch,
        e.target.value.length ? e.target.value : null
      );
    }
  })
)(Ads));

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

let LoggedInApp = () => {
  return (<div>
    <Route exact path="/facebook-ads/admin" component={Ads} /> {/* confusingly, despite being `exact`, this matches /facebook-ads/admin, without the trailing slash */}
    <Route exact path="/facebook-ads/admin/ads" component={Ads} />
    <Route exact path="/facebook-ads/admin/ads/" component={Ads} />
    <Route path="/facebook-ads/admin/ads/:ad_id" component={AdDetail} />
  </div>);
};

let App = ({ credentials }) => {
  return (<div id="app">
    <h1><Link to="/facebook-ads/admin">FBPAC Admin</Link></h1>
    
    {credentials && credentials.token ? <LoggedInApp /> : <Login />}
  </div>);
};
App = withRouter(connect((state) => state)(App));

go(() => {
  ReactDOM.render(
    <BrowserRouter>
      <Provider store={store}>
        <App />
      </Provider>
    </BrowserRouter>,
    document.querySelector("#react-root")
  );
});
