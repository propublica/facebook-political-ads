import "url-search-params-polyfill";
import i18next from "i18next";
import {
  batch,
  setLang,
  setTotal,
  setPage,
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

const serialize = state => {
  // N.B. this used to take store, now it takes state; so just give it store.getState()
  let params = new URLSearchParams();

  if (state.search) {
    params.set("search", state.search);
  }

  params = [serializeAdvertisers, serializeTargets, serializeEntities].reduce(
    (params, cb) => cb(params, state),
    params
  );

  if (state.pagination && state.pagination.page) {
    params.set("page", state.pagination.page);
  }

  if (state.lang !== i18next.language) {
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

  if (actions.length === 0) {
    actions.push(newSearch(""));
  }

  actions.push(setLang(params.get("lang") || "en-US"));
  return dispatch(batch(...actions));
};

export { auth, language, headers, serialize, deserialize };
