import React from "react";
import ReactDOM from "react-dom";
import { Router } from "react-router-dom";
import createHistory from "history/createBrowserHistory";
import { applyMiddleware, compose, combineReducers, createStore } from "redux";
import thunkMiddleware from "redux-thunk";
import persistState from "redux-localstorage";
import { Provider } from "react-redux";
import { createLogger } from "redux-logger";
import {
  entities,
  targets,
  advertisers,
  filters,
  pagination,
  search,
  summary,
  lang,
  ads,
  ad,
  enableBatching,
  groupedAttribute,
  politicalProbability,
  statesAndDistricts,
  states,
  parties,
  districts,
  by_state,
  yougov_only,
  no_listfund
} from "reducers.js";
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
    lang,
    summary,
    groupedAttribute,
    politicalProbability,
    statesAndDistricts,
    states,
    parties,
    districts,
    by_state,
    yougov_only,
    no_listfund
  })
);

const middleware = [thunkMiddleware, createLogger()];
const store = createStore(
  reducer,
  compose(...[persistState("credentials"), applyMiddleware(...middleware)])
);

go(() => {
  const history = createHistory();

  ReactDOM.render(
    <Router history={history}>
      <Provider store={store}>
        <Admin />
      </Provider>
    </Router>,
    document.querySelector("#react-root")
  );
});
