import React from "react";
import ReactDOM from "react-dom";
import { applyMiddleware, combineReducers, compose, createStore } from "redux";
import { Router } from "react-router-dom";
import history from "./history.js";
import thunkMiddleware from "redux-thunk";
import { Provider } from "react-redux";
import { createLogger } from "redux-logger";
import Frontend from "components/frontend.jsx";
import {
  ads,
  ad,
  search,
  entities,
  targets,
  advertisers,
  filters,
  pagination,
  lang
} from "reducers.js";
import { go } from "i18n.js";
import { enableBatching } from "reducers.js";

const reducer = enableBatching(
  combineReducers({
    ads,
    ad,
    search,
    entities,
    targets,
    advertisers,
    filters,
    pagination,
    lang
  })
);

const middleware = [thunkMiddleware, createLogger()];
const store = createStore(reducer, compose(applyMiddleware(...middleware)));

go(() => {
  ReactDOM.render(
    <Router history={history}>
      <Provider store={store}>
        <Frontend />
      </Provider>
    </Router>,
    document.querySelector("#graphic")
  );
  history.listen(() => {
    if (window.ga) {
      window.ga("send", "pageview");
    }
  });
});
