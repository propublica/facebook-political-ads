import { h, render } from 'preact';
import { applyMiddleware, combineReducers, compose, createStore } from 'redux';
import thunkMiddleware from 'redux-thunk';
import { Provider, connect } from 'preact-redux';
import { createLogger } from 'redux-logger';
import { NEW_ADS, search, refresh } from 'utils.js';
import throttle from "lodash/throttle";
import i18next from "i18next";
import Backend from 'i18next-xhr-backend';
import LanguageDetector from 'i18next-browser-languagedetector';
import { lastPage, pageIndex, pageCount } from 'pagination.js';

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
  search
});

const middleware = [thunkMiddleware, createLogger()];
const store = createStore(reducer, compose(applyMiddleware(...middleware)));
const div = document.createElement('div');

const cleanTargeting = (html) => {
  div.innerHTML = html;
  Array.from(div.querySelectorAll('a')).map((a) => a.remove());
  return Array.from(div.querySelectorAll('span:not(.hidden_elem)'))
    .reduce((memo, it) => memo + it.outerHTML, '');
};

const Ad = ({ ad }) => (
  <div className="message">
    <div dangerouslySetInnerHTML={{__html: ad.html}} />
  </div>
);

let Term = ({ search, term, onClick }) => (
  <li>
    <button
      type="button"
      className={term === search ? "prefab current" : "prefab"}
      onClick={function() { return onClick(store, term); }}
      value={term}>{term}</button>
  </li>
);
Term = connect(
  (state) => state,
  (dispatch) => ({ onClick: (store, term) => dispatch(refresh(store, term)) })
)(Term);

const Filters = () => (<div className="filters" />);
let Pagination = ({ pageIndex, prev, next }) => (
  <nav className="pagination">
    <ul>
      <li className="current">{pageIndex}</li>
    </ul>
  </nav>
);
Pagination = connect(
  (state) => state,
  (dispatch) => ({
    prev: () => {
      dispatch(pageCount.pagePrev());
      refresh(store);
    },
    next: (e) => {
      e.preventDefault();
      if (!store.getState().lastPage) {
        dispatch(pageCount.pageNext());
        refresh(store);
      }
    }
  })
);

let App = ({ads, onKeyUp, pageIndex, search}) => (
  <div id="app">
    <p dangerouslySetInnerHTML={{__html: t("guff")}} />
    <form id="facebook-pac-browser">
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
    onKeyUp: throttle((e) => {
      e.preventDefault();
      dispatch(pageCount.pageClear());
      dispatch(refresh(store, e.target.value.length ? e.target.value : null));
    }, 1000)
  })
)(App);

let t = null;
i18next
  .use(Backend)
  .use(LanguageDetector)
  .init({
    fallbackLng: 'en',
    backend: {
      loadPath: '/facebook-ads/locales/{{lng}}/{{ns}}.json'
    }
  }, (err, t_) => {
    t = t_;
    render(
      <Provider store={store}>
        <App />
      </Provider>,
      document.querySelector("#graphic")
    );
    store.dispatch(refresh(store));
  });
