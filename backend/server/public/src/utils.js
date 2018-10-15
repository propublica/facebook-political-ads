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
  newStates,
  filterState,
  newParties,
  filterParty,
  newDistricts,
  filterDistrict,
  filterbyPoliticalProbability,
  setPersona,
  showOldSearch,
  toggleYouGovOnly,
  toggleNoListfund
} from "actions.js";

const headers = lang => Object.assign({}, language(lang));

const language = lang => ({ "Accept-Language": lang + ";q=1.0" });

const serializeToJSON = (plural, singular, map) => {
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

const serializeEntities = serializeToJSON("entities", "entity", entity => ({
  entity
}));
const serializeAdvertisers = serializeToJSON("advertisers", "advertiser");

const serializeToArray = (plural, map) => {
  return (params, state) => {
    if (!state[plural] || state[plural].length == 0) return params;

    params.set(
      plural,
      state[plural].map(obj => (map ? map(obj) : obj)).join(",")
    );
    return params;
  };
};
const serializeStates = serializeToArray("states");
const serializeDistricts = serializeToArray("districts");
const serializeParties = serializeToArray("parties");

const serialize = state => {
  // N.B. this used to take store, now it takes state; so just give it store.getState()
  let params = new URLSearchParams();

  if (state.search) {
    params.set("search", state.search);
  }

  params = [
    serializeAdvertisers,
    serializeEntities,
    serializeStates,
    serializeParties,
    serializeDistricts
  ].reduce((params, cb) => cb(params, state), params);

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

  if (state.no_listfund) {
    params.set("no_listfund", "1");
  }

  if (state.yougov_only) {
    params.set("yougov_only", "1");
  }

  if (state.show_old_search) {
    params.set("showOldSearch", "1");
  }

  return params;
};

const deserialize = (dispatch, allowedLangs) => {
  const params = new URLSearchParams(location.search);
  const actions = [];
  if (params.has("search")) {
    actions.push(newSearch(params.get("search")));
  } else if (
    !(
      params.has("age_bucket") ||
      params.has("location_bucket") ||
      params.has("politics_bucket") ||
      params.has("gender")
    )
  ) {
    actions.push(newSearch(null)); // this resets the search in `state` if you click on a link from the Tools grouping pages.
  }

  if (params.has("showOldSearch")) {
    actions.push(showOldSearch());
  }

  if (params.has("no_listfund")) {
    actions.push(toggleNoListfund(true));
  }
  if (params.has("yougov_only")) {
    actions.push(toggleYouGovOnly(true));
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
      filterbyPoliticalProbability([
        parseInt(params.get("poliprob") || "70", 10),
        parseInt(params.get("maxpoliprob") || "100", 10)]
      )
    );
  }

  if (params.has("states")) {
    const states = params.get("states").split(",");
    actions.push(newStates(states));
    states.map(it => {
      actions.push(filterState(it));
    });
  }

  if (params.has("parties")) {
    const parties = params.get("parties").split(",");
    actions.push(newParties(parties));
    parties.map(it => {
      actions.push(filterParty(it));
    });
  }
  if (params.has("districts")) {
    const targeting = params.get("districts").split(",");
    actions.push(newDistricts(targeting));
    targeting.map(it => {
      actions.push(filterDistrict(it));
    });
  }

  if (
    params.has("age_bucket") ||
    params.has("location_bucket") ||
    params.has("politics_bucket") ||
    params.has("gender")
  ) {
    const persona = {};
    if (params.has("gender")) persona["gender"] = params.get("gender");
    if (params.has("politics_bucket"))
      persona["politics"] = params.get("politics_bucket");
    if (params.has("location_bucket"))
      persona["location"] = params.get("location_bucket").split(",");
    if (params.has("age_bucket")) persona["age"] = params.get("age_bucket");
    // if (params.has("age_bucket")) persona["name"] = params.get("persona");
    actions.push(setPersona(persona));
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
