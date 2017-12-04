import 'url-search-params-polyfill';
import { isLastPage, notLastPage } from 'pagination.js';
import { newAdvertisers, newEntities, newTargets } from 'filters.jsx';

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

const refresh = (store, query) => {
  return (dispatch) => {
    let url = "/facebook-ads/ads?";
    var params = new URLSearchParams();

    if(query) {
      params.append("search", query);
      dispatch(newSearch(query));

    }

    if (store.getState().pageIndex) {
      params.append("page", store.getState().pageIndex);
    }

    return fetch(`${url}${params.toString()}`, {
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
  };
};


export { headers, newAds, NEW_ADS, search, refresh };
