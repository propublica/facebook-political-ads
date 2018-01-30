import React from "react";
import ReactDOM from "react-dom";
import { applyMiddleware, compose, combineReducers, createStore } from "redux";
import thunkMiddleware from "redux-thunk";
import persistState from "redux-localstorage";
import { Provider } from "react-redux";
import { createLogger } from "redux-logger";
import { refresh, enableBatching, deserialize } from "utils.js";
import {
  entities,
  targets,
  advertisers,
  filters,
  pagination,
  search,
  lang,
  ads,
  ad,
  credentials
} from "reducers.js";
import { getOneAd } from "actions.js";
import { go } from "i18n.js";
import Admin from "components/admin/admin.jsx";

const reducer = enableBatching(
  combineReducers({
    ads,
    ad,
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

go(() => {
  ReactDOM.render(
    <Provider store={store}>
      <Admin />
    </Provider>,
    document.querySelector("#react-root")
  );

  deserialize(store.dispatch);
  // I'll figure out a better way to do this but I'm learning React so 🐻 with me.
  // we're probably going to want a router? maybe?
  const searchParams = new URLSearchParams(location.search);
  if (searchParams.get("detail")) {
    store.dispatch(getOneAd(searchParams.get("detail")));
    store.subscribe(() => refresh(store));
  } else {
    // anytime anything changes, then make the ajax request whenever the user changes the facets they
    // want.
    refresh(store).then(() => store.subscribe(() => refresh(store)));
  }
});
