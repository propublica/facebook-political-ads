import React from "react";
import { render } from "react-dom";
import thunkMiddleware from "redux-thunk";
import { applyMiddleware, compose, combineReducers, createStore } from "redux";
import { Provider } from "react-redux";
import persistState from "redux-localstorage";
import { createLogger } from "redux-logger";
import { newRatings, newAds, toggle } from "actions.js";
import { MOST_RECENT_STORIES_UPDATE } from "constants.js";
import { getAds, updateBadge, getUnratedRatings } from "utils.js";
import {
  active,
  ads,
  ratings,
  terms,
  thanks,
  language,
  ratings_count,
  stories_seen
} from "reducers.js";
import { Dispatcher } from "./components/dispatcher.jsx";
// styles
import "../css/styles.css";
import { ToggleType } from "./constants.js";

// The main reducer!
const reducer = combineReducers({
  active,
  ads,
  ratings,
  terms,
  thanks,
  language,
  ratings_count,
  stories_seen
});

let middleware = [thunkMiddleware];
if (process.env.NODE_ENV === "development") {
  middleware.push(createLogger());
}
const enhancer = compose(...[persistState(), applyMiddleware(...middleware)]);
let store = createStore(reducer, enhancer);

// this setTimeout is ugly but seems to be a workaround for https://bugs.chromium.org/p/chromium/issues/detail?id=649942
// we're trying to outsmart Chrome into thinking that our popup is "loaded" and ready to go
// otherwise it waits for all external assets to load before popping-up the popup.
setTimeout(
  () =>
    render(
      <Provider store={store}>
        <Dispatcher />
      </Provider>,
      document.body
    ),
  1
);

// connect to the ratings channel
chrome.runtime.onMessage.addListener(ads => store.dispatch(newRatings(ads)));
store.subscribe(() => {
  updateBadge(store.getState().ratings || [], store.getState().stories_seen);
});

// if an extension update has incremented MOST_RECENT_STORIES_UPDATE
// then switch the tab to the stories tab.
if (store.getState().stories_seen < MOST_RECENT_STORIES_UPDATE) {
  store.dispatch(toggle(ToggleType.STORIES, null));
}

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
