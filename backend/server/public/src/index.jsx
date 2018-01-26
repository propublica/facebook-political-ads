import React from "react";
import ReactDOM from "react-dom";
import { applyMiddleware, combineReducers, compose, createStore } from "redux";
import thunkMiddleware from "redux-thunk";
import { Provider, connect } from "react-redux";
import { createLogger } from "redux-logger";
import {
  NEW_ADS,
  search,
  refresh,
  newSearch,
  deserialize,
  enableBatching,
  lang
} from "utils.js";
import { debounce } from "lodash";
import { Filters, entities, targets, advertisers, filters } from "filters.jsx";
import { go, t } from "i18n.js";
import { Pagination, pagination } from "pagination.jsx";

const ads = (state = [], action) => {
  switch (action.type) {
    case NEW_ADS:
      return action.value;
    default:
      return state;
  }
};

const reducer = enableBatching(
  combineReducers({
    ads,
    search,
    entities,
    advertisers,
    targets,
    filters,
    pagination,
    lang
  })
);

const middleware = [thunkMiddleware, createLogger()];
const store = createStore(reducer, compose(applyMiddleware(...middleware)));

const Targeting = ({ targeting }) => (
  <div className="targeting_info">
    <div
      className="targeting"
      dangerouslySetInnerHTML={{
        __html: "<h3>Targeting Information</h3>" + targeting
      }}
    />
  </div>
);

const Ad = ({ ad }) => (
  <div>
    <div className="message">
      <div dangerouslySetInnerHTML={{ __html: ad.html }} />
    </div>
    {ad.targeting !== null ? <Targeting targeting={ad.targeting} /> : ""}
  </div>
);

let Term = ({ search, term, dispatch }) => (
  <li>
    <button
      type="button"
      className={term === search ? "prefab current" : "prefab"}
      onClick={() => dispatch(newSearch(term))}
      value={term}
    >
      {term}
    </button>
  </li>
);
Term = connect()(Term);

let App = ({ ads, onKeyUp, search }) => (
  <div id="app">
    <div dangerouslySetInnerHTML={{ __html: t("guff") }} />
    <form id="facebook-pac-browser" onSubmit={e => e.preventDefault()}>
      <fieldset className="prefabs">
        <legend>{t("search_terms")}</legend>
        <ul>
          {["Trump", "Obama", "Hillary", "Mueller", "Health", "Taxes"].map(
            term => <Term key={term} search={search} term={term} />
          )}
        </ul>
      </fieldset>
      <input
        type="search"
        id="search"
        placeholder={t("search")}
        onChange={onKeyUp}
        search={search}
      />
      <Filters />
    </form>
    <div className="facebook-pac-ads">
      {ads.length > 0 ? (
        <Pagination />
      ) : (
          <p className="no_ads">No ads found for {search}.</p>
        )}
      <div id="ads">{ads.map(ad => <Ad ad={ad} key={ad.id} />)}</div>
      {ads.length > 0 ? <Pagination /> : ""}
    </div>
  </div>
);
const throttledDispatch = debounce((dispatch, input) => {
  dispatch(newSearch(input));
}, 750);

App = connect(
  ({ ads, search }) => ({ ads, search }),
  dispatch => ({
    onKeyUp: e => {
      e.preventDefault();
      throttledDispatch(
        dispatch,
        e.target.value.length ? e.target.value : null
      );
    }
  })
)(App);

go(() => {
  ReactDOM.render(
    <Provider store={store}>
      <App />
    </Provider>,
    document.querySelector("#graphic")
  );

  deserialize(store.dispatch);
  refresh(store).then(() => store.subscribe(() => refresh(store)));
});
