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
  filterTarget,
  changePoliticalProbability
} from "actions.js";

const headers = lang => Object.assign({}, language(lang));

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

const serialize = state => {
  // N.B. this used to take store, now it takes state; so just give it store.getState()
  let params = new URLSearchParams();

  if (state.search) {
    params.set("search", state.search);
  }

  params = [serializeAdvertisers, serializeEntities].reduce(
    (params, cb) => cb(params, state),
    params
  );

  if (state.targets) {
    const items = state.targets.filter(it => it.active).map(it =>
      (({ target, segment }) => ({
        target,
        segment
      }))(it)
    );
    if (items.length > 0) params.set("targets", JSON.stringify(items));
  }

  if (state.pagination && state.pagination.page) {
    params.set("page", state.pagination.page);
  }

  if (state.politicalProbability && state.politicalProbability.length > 0) {
    params.set("poliprob", state.politicalProbability[0]);
    params.set("maxpoliprob", state.politicalProbability[1]);
  }

  if (state.lang && state.lang !== i18next.language) {
    params.set("lang", state.lang);
  }

  return params;
};

const deserialize = (dispatch, allowedLangs) => {
  const params = new URLSearchParams(location.search);
  const actions = [];
  if (params.has("search")) {
    actions.push(newSearch(params.get("search")));
  } else {
    actions.push(newSearch(null)); // this resets the search in `state` if you click on a link from the Tools grouping pages.
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
      advertiser => ({
        advertiser
      })
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

  if (params.has("poliprob") || params.has("maxpoliprob")) {
    actions.push(
      changePoliticalProbability(
        parseInt(params.get("poliprob") || "70", 10),
        parseInt(params.get("maxpoliprob") || "100", 10)
      )
    );
  }

  if (actions.length === 0) {
    actions.push(newSearch(""));
  }

  // if the allowedLangs param is set (it'll be set to [en-US, de-DE])
  // then we're going to give you the language you ask for iff it's in the array
  // otherwise -- only in admin -- we'll give you the lang you asked for
  // or the lang your browser asks for.

  if (allowedLangs) {
    if (allowedLangs.indexOf(params.get("lang")) > -1) {
      actions.push(setLang(params.get("lang")));
    } else if (allowedLangs.indexOf(i18next.language) > -1) {
      actions.push(setLang(i18next.language));
    } else {
      actions.push(setLang("en-US"));
    }
  } else {
    actions.push(setLang(params.get("lang") || i18next.language));
  }

  return dispatch(batch(...actions));
};

export { language, headers, serialize, deserialize };
