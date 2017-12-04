import 'url-search-params-polyfill';
import { isLastPage, notLastPage } from 'pagination.js';
import {
  newAdvertisers, newEntities, newTargets,
  serializeAdvertisers, serializeTargets, serializeEntities
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

  if(state.pageIndex) {
    params.set("page", state.pageIndex);
  }

  return params;
};
let loaded = false;
const refresh = (store) => {
  const url = "/facebook-ads/ads?";
  const dispatch = store.dispatch;
  const params = serialize(store, dispatch);
  const path = `${url}${params.toString()}`;
  const cleanSearch = (new URLSearchParams(window.location.search)).toString();
  if(!loaded || (cleanSearch !== params.toString())) {
    loaded = true;
    history.pushState({}, "", window.location.pathname + '?' + params.toString());
    return fetch(path, {
      method: "GET",
      headers: headers(store.getState().credentials)
    }).then((res) => res.json())
      .then((ads) => {
        if (ads.ads.length < 20) {
          dispatch(isLastPage());
        } else {
          dispatch(notLastPage());
        }
        dispatch(newAds(ads.ads));
        dispatch(newEntities(ads.entities));
        dispatch(newAdvertisers(ads.advertisers));
        dispatch(newTargets(ads.targets));
      });
  }
};


export { headers, newAds, NEW_ADS, search, refresh, newSearch };
