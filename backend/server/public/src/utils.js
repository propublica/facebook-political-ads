import "url-search-params-polyfill";
import { setTotal, setPage } from "pagination.jsx";
import {
  newAdvertisers,
  newEntities,
  newTargets,
  serializeAdvertisers,
  serializeTargets,
  serializeEntities,
  filterAdvertiser,
  filterEntity,
  filterTarget
} from "filters.jsx";

const auth = credentials =>
  credentials ? { Authorization: `Bearer ${credentials.token}` } : {};

const headers = (credentials, lang) =>
  Object.assign({}, auth(credentials), language(lang));

const language = lang => ({ "Accept-Language": lang + ";q=1.0" });

const NEW_ADS = "new_ads";
const newAds = ads => ({
  type: NEW_ADS,
  value: ads
});

const GOT_THAT_AD = "GOT_THAT_AD";
const receiveOneAd = ad => ({
  type: GOT_THAT_AD,
  ad: ad
});

const REQUESTING_ONE_AD = "REQUESTING_ONE_AD";
const requestingOneAd = ad_id => ({
  type: REQUESTING_ONE_AD,
  ad_id: ad_id
});

const SET_LANG = "set_lang";
const setLang = lang => ({
  type: SET_LANG,
  value: lang
});

const lang = (state = null, action) => {
  switch (action.type) {
    case SET_LANG:
      return action.value;
    default:
      return state;
  }
};

const NEW_SEARCH = "new_search";
const newSearch = query => ({
  type: NEW_SEARCH,
  value: query
});

const search = (state = null, action) => {
  switch (action.type) {
    case NEW_SEARCH:
      return action.value;
    default:
      return state;
  }
};

const serialize = store => {
  let params = new URLSearchParams();
  const state = store.getState();

  if (state.search) {
    params.set("search", state.search);
  }

  params = [serializeAdvertisers, serializeTargets, serializeEntities].reduce(
    (params, cb) => cb(params, state),
    params
  );

  if (state.pagination.page) {
    params.set("page", state.pagination.page);
  }

  if (state.lang === "de-DE") {
    params.set("lang", state.lang);
  }

  return params;
};

const deserialize = dispatch => {
  const params = new URLSearchParams(window.location.search);
  const actions = [];
  if (params.has("search")) {
    actions.push(newSearch(params.get("search")));
  }

  if (params.has("entities")) {
    const entities = JSON.parse(params.get("entities"));
    actions.push(newEntities(entities));
    entities.map(it => {
      actions.push(filterEntity(it));
    });
  }

  if (params.has("targets")) {
    const targeting = JSON.parse(params.get("targets"));
    actions.push(newTargets(targeting));
    targeting.map(it => {
      actions.push(filterTarget(it));
    });
  }

  if (params.has("advertisers")) {
    const advertisers = JSON.parse(params.get("advertisers")).map(
      advertiser => ({ advertiser })
    );
    actions.push(newAdvertisers(advertisers));
    advertisers.map(advertiser => {
      actions.push(filterAdvertiser(advertiser));
    });
  }

  if (params.has("page")) {
    actions.push(setTotal(10000));
    actions.push(setPage(parseInt(params.get("page"), 10)));
  }

  actions.push(setLang(params.get("lang") || "en-US"));

  dispatch(batch(...actions));
};

const BATCH = "batch";
const batch = (...actions) => {
  return {
    type: BATCH,
    actions: actions
  };
};

// https://github.com/reactjs/redux/issues/911#issuecomment-149192251
const enableBatching = reducer => {
  return function batchingReducer(state, action) {
    switch (action.type) {
      case BATCH:
        return action.actions.reduce(batchingReducer, state);
      default:
        return reducer(state, action);
    }
  };
};

// getOneAd() and refresh() are thunk action creators.
const getOneAd = (ad_id, url = "/facebook-ads/ads") => {
  if (!ad_id) return () => null;

  let path = `${url}/${ad_id}`;
  return (dispatch, getState) => {
    let state = getState();

    if (
      state.permalinked_ad &&
      state.permalinked_ad.ads &&
      state.permalinked_ad.ads[ad_id] &&
      state.permalinked_ad.ads[ad_id].id
    ) {
      return Promise.resolve(
        dispatch(receiveOneAd({ ads: [state.permalinked_ad.ads[ad_id]] }))
      );
    }
    dispatch(requestingOneAd(ad_id));

    fetch(path, {
      method: "GET",
      headers: headers(state.credentials, state.lang)
    })
      .then(res => res.json())
      .then(ads => {
        // it's really just one ad, but in an array...
        dispatch(receiveOneAd(ads.ads[0]));
      });
  };
};

// this is horrid, todo cleanup
let loaded = false;
const refresh = (store, url = "/facebook-ads/ads") => {
  const dispatch = store.dispatch;
  const params = serialize(store, dispatch);
  let path = `${url}?${params.toString()}`;
  const cleanSearch = new URLSearchParams(window.location.search);
  if (!loaded || cleanSearch.toString() !== params.toString()) {
    if (!loaded) {
      path = url + window.location.search;
    } else {
      if (cleanSearch.get("page") === params.get("page")) {
        params.delete("page");
      }
      let query = params.toString().length > 0 ? `?${params.toString()}` : "";
      if (!store.getState().permalinked_ad) {
        // if we're viewing a permalinked ad, don't change the query (but we can still do the refresh in teh backgorund.)
        history.pushState(
          { search: query },
          "",
          `${window.location.pathname}${query}`
        );
      }
    }
    return fetch(path, {
      method: "GET",
      headers: headers(store.getState().credentials, store.getState().lang)
    })
      .then(res => res.json())
      .then(ads => {
        dispatch(
          batch(
            newAds(ads.ads),
            newEntities(ads.entities),
            newAdvertisers(ads.advertisers),
            newTargets(ads.targets),
            setTotal(ads.total),
            setPage(parseInt(params.get("page"), 0) || 0)
          )
        );
        loaded = true;
      });
  }
};

export {
  headers,
  newAds,
  NEW_ADS,
  search,
  getOneAd,
  GOT_THAT_AD,
  REQUESTING_ONE_AD,
  COULDNT_GET_THAT_AD,
  refresh,
  newSearch,
  deserialize,
  enableBatching,
  lang
};
