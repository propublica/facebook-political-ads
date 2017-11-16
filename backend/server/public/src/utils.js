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

const INCREMENT = 'INCREMENT';
const DECREMENT = "DECREMENT";

const pageCount = {
  increment() {
    return { type: INCREMENT };
  },

  decrement() {
    return { type: DECREMENT };
  }
}

const refresh = (store, query, page) => {
  let url = "/facebook-ads/ads";
  if(query) {
    url = url + `?search=${query}`;
  }
  if(page) {
    url = url + `?page=${page}`;
  }
  return fetch(url, {
    method: "GET",
    headers: headers(store.getState().credentials)
  }).then((res) => res.json())
    .then((ads) => { store.dispatch(newAds(ads.ads)); });
};

const search = (store, query) => () => refresh(store, query, 0);

export { headers, newAds, NEW_ADS, pageCount, INCREMENT, DECREMENT, refresh, search };
