import 'url-search-params-polyfill';
import {isLastPage, notLastPage} from 'pagination.js'
const auth = (credentials) => (credentials ?
  {"Authorization": `Bearer ${credentials.token}`} :
  {});

const headers = (credentials) => Object.assign({}, auth(credentials), language());

const language = () => {
  const params = new URLSearchParams(location.search);
  if(params.get("lang")) {
    return {"Accept-Language": params.get("lang") + ";q=1.0"};
  } else {
    return {};
  }
};

const NEW_ADS = "new_ads";

const newAds = (ads) => ({
  type: NEW_ADS,
  value: ads
});

const refresh = (store, query) => {
  let url = "/facebook-ads/ads?";
  var params = new URLSearchParams ()
  if(query) {
    params.append("search", query);
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
        store.dispatch(isLastPage());
      } else {
        store.dispatch(notLastPage());
      }
      store.dispatch(newAds(ads.ads));
    });
};

const search = (store, query) => () => refresh(store, query, 0);

export { headers, newAds, NEW_ADS,  refresh, search };
