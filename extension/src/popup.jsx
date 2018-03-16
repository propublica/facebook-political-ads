import React from "react";
import { render } from "react-dom";
import thunkMiddleware from "redux-thunk";
import { applyMiddleware, compose, combineReducers, createStore } from "redux";
import { Provider } from "react-redux";
import persistState from "redux-localstorage";
import { createLogger } from "redux-logger";
import { newRatings, newAds } from "actions.js";
import { getAds, updateBadge, getUnratedRatings } from "utils.js";
import {
  active,
  ads,
  ratings,
  terms,
  thanks,
  language,
  ratings_count
} from "reducers.js";
import { Dispatcher } from "./components/dispatcher.jsx";
// styles
import "../css/styles.css";

// The main reducer!
const reducer = combineReducers({
  active,
  ads,
  ratings,
  terms,
  thanks,
  language,
  ratings_count
});

let middleware = [thunkMiddleware];
if (process.env.NODE_ENV === "development") {
  middleware.push(createLogger());
}
const enhancer = compose(...[persistState(), applyMiddleware(...middleware)]);
let store = createStore(reducer, enhancer);

render(
  <Provider store={store}>
    <div id="popup">
      <Dispatcher />
    </div>
  </Provider>,
  document.body
);

// connect to the ratings channel
chrome.runtime.onMessage.addListener(ads => store.dispatch(newRatings(ads)));
store.subscribe(() => updateBadge(store.getState().ratings || []));

// Refresh our ads by first filtering out ones the user has seen, and then merging like with
// ratings.
if (store.getState().terms) {
  getAds(store.getState().language, resp => {
    const set = new Set();
    getUnratedRatings(store.getState().ratings).map(rating =>
      set.add(rating.id)
    );
    store.dispatch(newAds(resp.filter(ad => !set.has(ad.id))));
  });
}
