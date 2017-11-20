import { h, render } from 'preact';
import { applyMiddleware, combineReducers, compose, createStore } from 'redux';
import thunkMiddleware from 'redux-thunk';
import { Provider, connect } from 'preact-redux';
import { createLogger } from 'redux-logger';
import { PAGE_PREV, PAGE_NEXT, NEW_ADS, pageCount, refresh, search } from 'utils.js';
import throttle from "lodash/throttle";
import i18next from "i18next";
import Backend from 'i18next-xhr-backend';
import LanguageDetector from 'i18next-browser-languagedetector';

const ads = (state = [], action) => {
  switch(action.type) {
  case NEW_ADS:
    return action.value;
  default:
    return state;
  }
};

const pageIndex = (state = 0, action) => {
  switch(action.type) {
    case PAGE_NEXT:
      return state + 1;

    case PAGE_PREV:
      return state - 1;

    default:
      return state;
  }
}

const reducer = combineReducers({
  ads, 
  pageIndex
});

const middleware = [thunkMiddleware, createLogger()];
const store = createStore(reducer, compose(applyMiddleware(...middleware)));
const div = document.createElement('div');

const cleanTargeting = (html) => {
  div.innerHTML = html;
  Array.from(div.querySelectorAll('a')).map((a) => a.remove());
  return Array.from(div.querySelectorAll('span:not(.hidden_elem)')).reduce((memo, it) => memo + it.outerHTML, '');
};

const killImages = (html) => {
  div.innerHTML = html;
  Array.from(div.querySelectorAll('img')).map((a) => a.remove());
  return div.outerHTML;
};

const Ad = ({ ad }) => (
  <div className="message cf">
    <div className="container">
      <div className="chiclet">
        {ad.thumbnail ? <img src={ad.thumbnail} /> : ''}
      </div>
      <div className="display">
        <div className="title">{ad.title}</div>
        <div className="content" dangerouslySetInnerHTML={{__html: killImages(ad.message) }} />
      </div>
    </div>
    {ad.images.length > 0 ?
      <div className="images">
        {ad.images.map((src) => <img src={src} key={src} />)}
      </div> :
      ''}
    {ad.targeting ?
      <div className="targeting">
        <h3>{t("targeting")}</h3>
        <div dangerouslySetInnerHTML={{__html: cleanTargeting(ad.targeting) }} />
      </div> :
      ''}
  </div>
);

let App = ({ads, onKeyUp}) => (
  <div id="app">
    <h1>{t("title")}</h1>
    <h2>{t("slug")}</h2>
    <p id="byline">{t("by")} Jeff Larson {t("and")} Julia Angwin, ProPublica, September 22, 2017</p>
    <p dangerouslySetInnerHTML={{__html: t("guff")}} />
    <input id="search" placeholder={t("search")} onKeyUp={onKeyUp} />
    <div id="ads">
      {ads.map((ad) => <Ad ad={ad} key={ad.id} />)}
    </div>
    <div id="pageNav">
      <div id="previous"><a href="" onClick={pageCount.pagePrev}>Previous</a></div> 
      <div id="next"><a href="" onClick={pageCount.pageNext}>Next</a></div>
    </div>
  </div>
);
App = connect(
  (state) => state,
  (dispatch) => ({
    onKeyUp: throttle((e) => {
      e.preventDefault();
      dispatch(search(store, e.target.value.length ? e.target.value : null));
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
    refresh(store);
  });
