import 'url-search-params-polyfill';
import { isLastPage, notLastPage, pageCount } from 'pagination.js';
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

const deserialize = (dispatch) => {
  const params = new URLSearchParams(window.location.search);
  if(params.has("search")) {
    dispatch(newSearch(params.get("search")));
  }

  if(params.has("entities")) {
    const entities = JSON.parse(params.get("entities"));
    dispatch(newEntities(entities));
    entities.map((it) => {
      dispatch(filterEntity(it));
    });
  }

  if(params.has("targeting")) {
    const targeting = JSON.parse(params.get("targeting"));
    dispatch(newTargets(targeting));
    targeting.map((it) => {
      dispatch(filterTarget(it));
    });
  }

  if(params.has("advertiser")) {
    const advertisers = JSON.parse(params.get("advertiser")).map((advertiser) => ({ advertiser }));
    dispatch(newAdvertisers(advertisers));
    advertisers.map((advertiser) => {
      dispatch(filterAdvertiser(advertiser));
    });
  }

  if(params.has("page")) {
    dispatch(pageCount.setPage(parseInt(params.get("page"), 10)));
  }
};


let loaded = false;
const refresh = (store) => {
  const url = "/facebook-ads/ads";
  const dispatch = store.dispatch;
  const params = serialize(store, dispatch);
  let path = `${url}?${params.toString()}`;
  const cleanSearch = (new URLSearchParams(window.location.search)).toString();
  if(!loaded || (cleanSearch !== params.toString())) {
    if(!loaded) {
      path = url + window.location.search;
    } else {
      history.pushState({}, "", window.location.pathname + '?' + params.toString());
    }
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
        loaded = true;
      });
  }
};

export { headers, newAds, NEW_ADS, search, refresh, newSearch, deserialize };
