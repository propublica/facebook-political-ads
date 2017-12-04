import { h, render } from 'preact';
import { applyMiddleware, combineReducers, compose, createStore } from 'redux';
import thunkMiddleware from 'redux-thunk';
import { Provider, connect } from 'preact-redux';
import { createLogger } from 'redux-logger';
import { NEW_ADS, search, refresh, newSearch, deserialize } from 'utils.js';
import { debounce } from "lodash";
import { Filters, entities, targets, advertisers, filters } from 'filters.jsx';
import { go, t } from 'i18n.js';
import { lastPage, pageIndex, pageCount } from 'pagination.js';
import { range } from 'lodash';

const ads = (state = [], action) => {
  switch(action.type) {
  case NEW_ADS:
    return action.value;
  default:
    return state;
  }
};

const reducer = combineReducers({
  ads,
  pageIndex,
  lastPage,
  search,
  entities,
  advertisers,
  targets,
  filters
});

const middleware = [thunkMiddleware, createLogger()];
const store = createStore(reducer, compose(applyMiddleware(...middleware)));
const div = document.createElement('div');

const Targeting = ({ targeting }) => (
  <div className="targeting_info">
    <h3>Targeting Information</h3>
    <div
      className="targeting"
      dangerouslySetInnerHTML={{__html:targeting}} />
  </div>
);

const Ad = ({ ad }) => (
  <div>
    <div className="message">
      <div dangerouslySetInnerHTML={{__html: ad.html}} />
    </div>
    {ad.targeting !== null ? <Targeting targeting={ad.targeting} /> : ''}
  </div>
);

let Term = ({ search, term, dispatch }) => (
  <li>
    <button
      type="button"
      className={term === search ? "prefab current" : "prefab"}
      onClick={() => dispatch(newSearch(term)) }
      value={term}>{term}</button>
  </li>
);
Term = connect(
  () => ({})
)(Term);

let Pagination = ({ pageIndex, prev, next, set }) => (
  <nav className="pagination">
    <ul>
      <li><a href="" onClick={prev}>←</a></li>
      {range(Math.max(0, pageIndex - 2), pageIndex + 3).map((i) => {
        return (i === pageIndex ?
          <li key={i} className="current">{pageIndex + 1}</li> :
          <li key={i} ><a href="" onClick={(e) => set(e, i + 1)}>{i + 1}</a></li>);
      })}
      <li><a href="" onClick={next}>→</a></li>
    </ul>
  </nav>
);
Pagination = connect(
  (state) => state,
  (dispatch) => ({
    prev: (e) => {
      e.preventDefault();
      dispatch(pageCount.pagePrev());
    },
    next: (e) => {
      e.preventDefault();
      if(!store.getState().lastPage) {
        dispatch(pageCount.pageNext());
      }
    },
    set: (e, i) => {
      e.preventDefault();
      dispatch(pageCount.setPage(i));
    }
  })
)(Pagination);

let App = ({ads, onKeyUp, pageIndex, search}) => (
  <div id="app">
    <p dangerouslySetInnerHTML={{__html: t("guff")}} />
    <form id="facebook-pac-browser" onSubmit={(e) => e.preventDefault()}>
      <fieldset className="prefabs">
        <legend>{t("search_terms")}</legend>
        <ul>
          {["Trump", "Obama", "Hillary", "Mueller", "Health", "Taxes"]
            .map((term) => <Term key={term} search={search} term={term} />)}
        </ul>
      </fieldset>
      <input type="search" id="search" placeholder={t("search")} onKeyUp={onKeyUp} value={search} />
      <Filters />
    </form>
    <div className="facebook-pac-ads">
      <Pagination page={pageIndex} />
      <div id="ads">
        {ads.map((ad) => <Ad ad={ad} key={ad.id} />)}
      </div>
      <Pagination page={pageIndex} />
    </div>
  </div>
);
App = connect(
  (state) => state,
  (dispatch) => ({
    onKeyUp: debounce((e) => {
      e.preventDefault();
      dispatch(pageCount.setPage(0));
      dispatch(newSearch(e.target.value.length ? e.target.value : null));
    }, 750)
  })
)(App);

go(() => {
  render(
    <Provider store={store}>
      <App />
    </Provider>,
    document.querySelector("#graphic")
  );

  deserialize(store.dispatch);
  refresh(store).then(() => store.subscribe(() => refresh(store)));
});
