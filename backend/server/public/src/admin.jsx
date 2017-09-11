import { h, render } from 'preact';
import { applyMiddleware, compose, combineReducers, createStore } from 'redux';
import thunkMiddleware from 'redux-thunk';
import persistState from 'redux-localstorage';
import { Provider, connect } from 'preact-redux';
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

let Login = (onLogin) => (
  <form id="login">
    <input id="username" type="text" />
    <input id="password" type="password" />
    <input id="submit" type="submit" onClick={onLogin} />
  </form>
);
Login = connect(
  (state) => state,
  (dispatch) => ({
    onLogin: (e) => {
      e.preventDefault();
      console.log(e);
      dispatch(authorize('j', 'j'));
    }
  })
);

let Ad = (ad) => (
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
        <td>thumbnail</td>
        <td>{ad.thumbnail}</td>
      </tr>
      <tr>
        <td>images</td>
        <td>{ad.images.map((src) => <img key={src} src={src} />)}</td>
      </tr>
      <tr>
        <td>text</td>
        <td dangerouslySetInnerHTML={{__html: ad.html}} />
      </tr>
    </table>
  </div>
);
Ad = connect(
  (state) => state,
  (dispatch) => ({
    onClick: (ad) => dispatch(suppressAd(ad))
  })
);

const Ads = (ads) => (
  <div id="ads">
    {ads.map((ad) => <Ad ad={ad} key={ad.id} />)}
  </div>
);

const App = ({ads, credentials}) => (
  <div id="app">
    {credentials ? <Ads ads={ads} /> : <Login />}
  </div>
);


render(
  <Provider store={store}>
    <App />
  </Provider>,
  document.body
);
