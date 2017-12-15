import 'url-search-params-polyfill';
import { setTotal, setPage } from 'pagination.jsx';
import {
  newAdvertisers, newEntities, newTargets,
  serializeAdvertisers, serializeTargets, serializeEntities,
  filterAdvertiser, filterEntity, filterTarget
} from 'filters.jsx';

const auth = (credentials) => (credentials ?
  {"Authorization": `Bearer ${credentials.token}`} :
  {});

const headers = (credentials) => Object.assign({}, auth(credentials), language());

const language = () => {
  const params = new URLSearchParams(location.search);
  if(params.get("lang")) {
    return {"Accept-Language": params.get("lang") + ";q=1.0"};
  } else if(!["en-US", "de-DE"].includes(navigator.langage)) {
    return {"Accept-Language": "en-US;q=1.0"};
  }
};

const NEW_ADS = "new_ads";

const newAds = (ads) => ({
  type: NEW_ADS,
  value: ads
});

const SET_LANG = "set_lang";
const setLang = (lang) => ({
  type: SET_LANG,
  value: lang
});

const lang = (state = null, action) => {
  switch(action.type) {
  case SET_LANG:
    return action.value;
  default:
    return state;
  }
};

const NEW_SEARCH = "new_search";
const newSearch = (query) => ({
  type: NEW_SEARCH,
  value: query
});

const search = (state = null, action) => {
  switch(action.type) {
  case NEW_SEARCH:
    return action.value;
  default:
    return state;
  }
};

const serialize = (store) => {
  let params = new URLSearchParams();
  const state = store.getState();

  if(state.search) {
    params.set("search", state.search);
  }

  params = [serializeAdvertisers, serializeTargets, serializeEntities]
    .reduce((params, cb) => cb(params, state), params);

  if(state.pagination.page) {
    params.set("page", state.pagination.page);
  }

  if(state.lang) {
    params.set("lang", state.lang);
  }

  return params;
};

const deserialize = (dispatch) => {
  const params = new URLSearchParams(window.location.search);
  const actions = [];
  if(params.has("search")) {
    actions.push(newSearch(params.get("search")));
  }

  if(params.has("entities")) {
    const entities = JSON.parse(params.get("entities"));
    actions.push(newEntities(entities));
    entities.map((it) => {
      actions.push(filterEntity(it));
    });
  }

  if(params.has("targets")) {
    const targeting = JSON.parse(params.get("targets"));
    actions.push(newTargets(targeting));
    targeting.map((it) => {
      actions.push(filterTarget(it));
    });
  }

  if(params.has("advertisers")) {
    const advertisers = JSON.parse(params.get("advertisers")).map((advertiser) => ({ advertiser }));
    actions.push(newAdvertisers(advertisers));
    advertisers.map((advertiser) => {
      actions.push(filterAdvertiser(advertiser));
    });
  }

  if(params.has("page")) {
    actions.push(setPage(parseInt(params.get("page"), 10)));
  }

  if(params.has("lang")) {
    actions.push(setLang(params.get("lang")));
  }

  dispatch(batch(...actions));
};

const BATCH = 'batch';
const batch = (...actions) => {
  return {
    type: BATCH,
    actions: actions
  };
};

// https://github.com/reactjs/redux/issues/911#issuecomment-149192251
const enableBatching = (reducer) => {
  return function batchingReducer(state, action) {
    switch (action.type) {
    case BATCH:
      return action.actions.reduce(batchingReducer, state);
    default:
      return reducer(state, action);
    }
  };
};

// this is horrid, todo cleanup
let loaded = false;
const refresh = (store, url = "/facebook-ads/ads") => {
  const dispatch = store.dispatch;
  const params = serialize(store, dispatch);
  let path = `${url}?${params.toString()}`;
  const cleanSearch = (new URLSearchParams(window.location.search));
  if(!loaded || (cleanSearch.toString() !== params.toString())) {
    if(!loaded) {
      path = url + window.location.search;
    } else {
      if(cleanSearch.get("page") === params.get("page")) {
        params.delete("page");
      }
      let query = params.toString().length > 0 ? `?${params.toString()}` : '';
      history.pushState({}, "", `${window.location.pathname}${query}`);
    }
    return fetch(path, {
      method: "GET",
      headers: headers(store.getState().credentials)
    }).then((res) => res.json())
      .then((ads) => {
        dispatch(batch(
          newAds(ads.ads),
          newEntities(ads.entities),
          newAdvertisers(ads.advertisers),
          newTargets(ads.targets),
          setTotal(ads.total),
          setPage(parseInt(params.get("page"), 0) || 0)
        ));
        loaded = true;
      });
  }
};

export { headers, newAds, NEW_ADS, search, refresh, newSearch, deserialize, enableBatching, lang };
