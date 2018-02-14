import "url-search-params-polyfill";
import {
  batch,
  setLang,
  setTotal,
  setPage,
  newAds,
  newAdvertisers,
  newEntities,
  newSearch,
  newTargets,
  filterAdvertiser,
  filterEntity,
  filterTarget
} from "actions.js";

const auth = credentials =>
  credentials ? { Authorization: `Bearer ${credentials.token}` } : {};

const headers = (credentials, lang) =>
  Object.assign({}, auth(credentials), language(lang));

const language = lang => ({ "Accept-Language": lang + ";q=1.0" });

const s = (plural, singular, map) => {
  return (params, state) => {
    if (!state[plural]) return params;
    const items = state[plural]
      .filter(it => it.active)
      .map(it => (map ? map(it[singular]) : it[singular]));
    if (items.length > 0) {
      params.set(plural, JSON.stringify(items));
    }
    return params;
  };
};

const serializeEntities = s("entities", "entity", entity => ({ entity }));
const serializeAdvertisers = s("advertisers", "advertiser");
const serializeTargets = s("targets", "target", target => ({ target }));

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
  const params = new URLSearchParams(location.search);
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

  return dispatch(batch(...actions));
};

// this is horrid, todo cleanup
let loaded = false;
const refresh = (store, url = "/facebook-ads/ads") => {
  const dispatch = store.dispatch;
  const params = serialize(store, dispatch);
  let path = `${url}?${params.toString()}`;
  const cleanSearch = new URLSearchParams(location.search);
  if (!loaded || cleanSearch.toString() !== params.toString()) {
    if (!loaded) {
      path = url + window.location.search;
    } else {
      if (cleanSearch.get("page") === params.get("page")) {
        params.delete("page");
      }
      let query = params.toString().length > 0 ? `?${params.toString()}` : "";
      history.pushState({ search: query }, "", `${location.pathname}${query}`);
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
  } else {
    return Promise.resolve();
  }
};

export { auth, language, headers, refresh, serialize, deserialize };
