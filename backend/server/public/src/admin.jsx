import { h, render } from 'preact';
import { applyMiddleware, compose, combineReducers, createStore } from 'redux';
import thunkMiddleware from 'redux-thunk';
import persistState from 'redux-localstorage';
import { Provider, connect } from 'preact-redux';
import { createLogger } from 'redux-logger';

const NEW_ADS = "new_ads";
const HIDE_AD = "hide_ad";
const LOGIN = "login";
const LOGOUT = "logout";

const auth = (credentials) => (credentials ?
  {"Authorization": `Bearer ${credentials.token}`} :
  {});

const language = () => {
  const params = new URLSearchParams(location.search);
  if(params.get("lang")) {
    return {"Accept-Language": params.get("lang") + ";q=1.0"};
  } else {
    return {};
  }
};

const headers = (credentials) => Object.assign({}, auth(credentials), language());

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

const refresh = (getState) => fetch("/facebook-ads/ads", {
  method: "GET",
  headers: headers(getState().credentials)
}).then((res) => res.json())
  .then((ads) => store.dispatch(newAds(ads)));

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

const newAds = (ads) => ({
  type: NEW_ADS,
  value: ads
});

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

const reducer = combineReducers({
  credentials,
  ads,
});
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
        <td colSpan="2">
          {ad.suppressed ? "Suppressed" :
            <button onClick={function() { return onClick(ad); }}>
              Suppress
            </button>}
        </td>
      </tr>
    </table>
  </div>
);

let Ads = ({ads, onClick}) => (
  <div id="ads">
    {ads.map((ad) => <Ad ad={ad} key={ad.id} onClick={onClick} />)}
  </div>
);
Ads = connect(
  (state) => ({
    ads: state.ads.filter((ad) => !ad.suppressed)
  }),
  (dispatch) => ({
    onClick: (ad) => dispatch(suppressAd(ad))
  })
)(Ads);

let App = ({credentials}) => (
  <div id="app">
    {credentials && credentials.token ? <Ads /> : <Login />}
  </div>
);
App = connect((state) => state)(App);

let Login = ({onLogin}) => (
  <form id="login" onSubmit={onLogin} >
    <input id="email" type="text" placeholder="email" />
    <input id="password" type="password" placeholder="password" />
    <input id="submit" type="submit" value="login" />
  </form>
);
Login = connect(
  (state) => state,
  (dispatch) => ({
    onLogin: (e) => {
      e.preventDefault();
      dispatch(authorize(
        e.target.querySelector("#email").value,
        e.target.querySelector("#password").value
      ));
    }
  })
)(Login);

render(
  <Provider store={store}>
    <App />
  </Provider>,
  document.body
);
refresh(store.getState);
