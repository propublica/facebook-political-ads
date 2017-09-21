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
  let url = "/facebook-ads/ads";
  if(query) {
    url = url + `?search=${query}`;
  }
  return fetch(url, {
    method: "GET",
    headers: headers(store.getState().credentials)
  }).then((res) => res.json())
    .then((ads) => store.dispatch(newAds(ads)));
};

const search = (store, query) => () => refresh(store, query);

export { headers, newAds, NEW_ADS, refresh, search };
