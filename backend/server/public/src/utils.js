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

const PAGE_NEXT = 'PAGE_NEXT';
const PAGE_PREV = 'PAGE_PREV';
// SM: Guessing we need this for clearing pages when you search. Haven't implemneted yet.
const PAGE_CLEAR = 'PAGE_CLEAR';

const pageCount = {
  pageNext() {
    return { type: PAGE_NEXT };
  },
  pagePrev() {
    return { type: PAGE_PREV };
  },
  pageClear() {
    return { type: PAGE_CLEAR}
  }
}

const IS_LAST_PAGE = 'IS_LAST_PAGE';
const NOT_LAST_PAGE = 'NOT_LAST_PAGE';

const isLastPage = () => ({
  type: IS_LAST_PAGE
});

const notLastPage = () => ({
  type: NOT_LAST_PAGE
})

const refresh = (store, query, page) => {
  let url = "/facebook-ads/ads";
  if(query) {
    url = url + `?search=${query}`;
  }
  if (store.getState().pageIndex) {
    url = url + `?page=${store.getState().pageIndex}`;
  }

  return fetch(url, {
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

export { headers, newAds, NEW_ADS, pageCount, PAGE_NEXT, PAGE_PREV, PAGE_CLEAR, IS_LAST_PAGE, NOT_LAST_PAGE, refresh, search };
