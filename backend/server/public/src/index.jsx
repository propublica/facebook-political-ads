import React from "react";
import ReactDOM from "react-dom";
import { applyMiddleware, combineReducers, compose, createStore } from "redux";
import thunkMiddleware from "redux-thunk";
import { Provider } from "react-redux";
import { createLogger } from "redux-logger";
import Frontend from "components/frontend.jsx";
import {
  ads,
  search,
  entities,
  targets,
  advertisers,
  filters,
  pagination,
  lang
} from "reducers.js";
import { refresh, deserialize } from "utils.js";
import { go } from "i18n.js";
import { enableBatching } from "reducers.js";

const reducer = enableBatching(
  combineReducers({
    ads,
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
    <Provider store={store}>
      <Frontend />
    </Provider>,
    document.querySelector("#graphic")
  );

  deserialize(store.dispatch);
  refresh(store).then(() => store.subscribe(() => refresh(store)));
});
