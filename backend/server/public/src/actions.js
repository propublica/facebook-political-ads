import { serialize } from "utils.js";
import { debounce } from "lodash";
import history from "./history.js";

export const URL_ROOT =
  window.location.href.indexOf("localhost") > -1
    ? "http://localhost:3000"
    : window.location.host.indexOf("10.55.") > -1
      ? "http://" + window.location.host.split(":")[0] + ":3000"
      : "";

export const NEW_ADS = "new_ads";
export const newAds = ads => ({
  type: NEW_ADS,
  value: ads
});

export const GOT_THAT_AD = "GOT_THAT_AD";
export const receiveOneAd = ad => ({
  type: GOT_THAT_AD,
  ad: ad
});

export const REQUESTING_ONE_AD = "REQUESTING_ONE_AD";
export const requestingOneAd = ad_id => ({
  type: REQUESTING_ONE_AD,
  ad_id: ad_id
});

export const GOT_RECENT_GROUPED_ATTR = "GOT_RECENT_GROUPED_ATTR";
export const receiveRecentGroupedAttr = groupedAttrs => ({
  type: GOT_RECENT_GROUPED_ATTR,
  groupedAttrs
});

export const REQUESTING_RECENT_GROUPED_ATTR = "REQUESTING_RECENT_GROUPED_ATTR";
export const requestingRecentGroupedAttr = () => ({
  type: REQUESTING_RECENT_GROUPED_ATTR
});

export const RECEIVE_STATES_AND_DISTRICTS = "RECEIVE_STATES_AND_DISTRICTS";
export const receiveStatesAndDistricts = statesAndDistricts => ({
  type: RECEIVE_STATES_AND_DISTRICTS,
  statesAndDistricts
});

export const REQUESTING_STATES_AND_DISTRICTS =
  "REQUESTING_STATES_AND_DISTRICTS";
export const requestingStatesAndDistricts = () => ({
  type: REQUESTING_STATES_AND_DISTRICTS
});

export const GOT_ADMIN_SUMMARY = "GOT_ADMIN_SUMMARY";
export const receivedAdminSummary = summary => ({
  type: GOT_ADMIN_SUMMARY,
  summary
});

export const REQUESTING_ADMIN_SUMMARY = "REQUESTING_ADMIN_SUMMARY";
export const requestingAdminSummary = () => ({
  type: REQUESTING_ADMIN_SUMMARY
});

export const GOT_HOMEPAGE_SUMMARY = "GOT_HOMEPAGE_SUMMARY";
export const receivedHomepageSummary = summary => ({
  type: GOT_HOMEPAGE_SUMMARY,
  summary
});

export const REQUESTING_HOMEPAGE_SUMMARY = "REQUESTING_HOMEPAGE_SUMMARY";
export const requestingHomepageSummary = () => ({
  type: REQUESTING_HOMEPAGE_SUMMARY
});

export const SET_LANG = "set_lang";
export const setLang = lang => ({
  type: SET_LANG,
  value: lang
});

export const NEW_SEARCH = "new_search";
export const newSearch = query => ({
  type: NEW_SEARCH,
  value: query
});

export const CLEAR_PERSONA = "clear_persona";
export const clearPersona = () => ({
  type: CLEAR_PERSONA
});
export const SET_PERSONA = "set_persona";
export const setPersona = persona => ({
  type: SET_PERSONA,
  value: persona
});

export const SET_PERSONA_FACET = "set_persona_facet";
export const setPersonaFacet = (facet_key, facet_val) => ({
  type: SET_PERSONA_FACET,
  value: { [facet_key]: facet_val }
});

const asyncResetPage = action => {
  return (dispatch, getState) => {
    dispatch(setPage(0));
    return async(action)(dispatch, getState);
  };
};

const async = (action, cb) => {
  return (dispatch, getState) => {
    dispatch(action);
    return cb ? cb(dispatch, getState) : getAds()(dispatch, getState);
  };
};
export const fetchSearch = query => asyncResetPage(newSearch(query));

export const throttledDispatch = debounce((dispatch, input) => {
  dispatch(fetchSearch(input));
}, 750);
// throttledDispatchAny(dispatch, fetchSearch, input) // TODO

export const throttledDispatchAny = debounce((dispatch, func, input) => {
  dispatch(func(input));
}, 750);

export const BATCH = "batch";
export const batch = (...actions) => ({
  type: BATCH,
  actions: actions
});

const a = type => arg => ({ type, value: arg });
export const NEW_ENTITIES = "new_entities";
export const NEW_ADVERTISERS = "new_advertisers";
export const NEW_TARGETS = "new_targets";
export const NEW_STATES = "new_states";
export const NEW_PARTIES = "new_parties";
export const NEW_DISTRICTS = "new_districts";
export const NEW_BY_STATE = "new_by_state"; // this is for the combined state page: ads targeted TO the state, ads mentioning or by candidates in the state
export const newEntities = a(NEW_ENTITIES);
export const newAdvertisers = a(NEW_ADVERTISERS);
export const newTargets = a(NEW_TARGETS);
export const newStates = a(NEW_STATES);
export const newParties = a(NEW_PARTIES);
export const newDistricts = a(NEW_DISTRICTS);
export const newByState = a(NEW_BY_STATE);

// resets advertisers, targets, entities, states, districts, parties
export const CLEAR_ALL_FILTERS = "clear_all_filters";
export const clearAllFilters = () => ({
  type: CLEAR_ALL_FILTERS
});

export const FILTER_ENTITY = "filter_entity";
export const FILTER_ADVERTISER = "filter_advertiser";
export const FILTER_TARGET = "filter_target";
export const FILTER_STATE = "filter_states";
export const FILTER_BY_BY_STATE = "filter_by_by_state"; // this is for the combined state page: ads targeted TO the state, ads mentioning or by candidates in the state
export const FILTER_PARTY = "filter_parties";
export const FILTER_DISTRICT = "filter_districts";
export const filterEntity = a(FILTER_ENTITY);
export const filterAdvertiser = a(FILTER_ADVERTISER);
export const filterTarget = a(FILTER_TARGET);
export const filterState = a(FILTER_STATE);
export const filterByByState = a(FILTER_BY_BY_STATE); // this is for the combined state page: ads targeted TO the state, ads mentioning or by candidates in the state

export const filterParty = a(FILTER_PARTY);
export const filterDistrict = a(FILTER_DISTRICT);
export const fetchEntity = e => asyncResetPage(filterEntity(e));
export const fetchAdvertiser = a => asyncResetPage(filterAdvertiser(a));
export const fetchTarget = t => asyncResetPage(filterTarget(t));
export const fetchState = e => asyncResetPage(filterState(e));
export const fetchParty = a => asyncResetPage(filterParty(a));
export const fetchDistrict = t => asyncResetPage(filterDistrict(t));

export const TOGGLE_TARGET = "toggle_target";
export const TOGGLE_ADVERTISER = "toggle_advertiser";
export const TOGGLE_ENTITY = "toggle_entity";
export const RESET_DROPDOWNS = "reset_dropdowns";
export const toggleTarget = () => ({ type: TOGGLE_TARGET });
export const toggleAdvertiser = () => ({ type: TOGGLE_ADVERTISER });
export const toggleEntity = () => ({ type: TOGGLE_ENTITY });
export const resetDropdowns = () => ({ type: RESET_DROPDOWNS });

export const CHANGE_POLITICAL_PROBABILITY = "change_poliprob";
export const filterbyPoliticalProbability = a(CHANGE_POLITICAL_PROBABILITY);
export const changePoliticalProbability = t =>
  asyncResetPage(filterbyPoliticalProbability(t));

export const SHOW_OLD_SEARCH = "show_old_search";
export const HIDE_OLD_SEARCH = "hide_old_search";
export const showOldSearch = () => ({ type: SHOW_OLD_SEARCH });
export const hideOldSearch = () => ({ type: HIDE_OLD_SEARCH });

export const NEXT_PAGE = "next_page";
export const PREV_PAGE = "prev_page";
export const SET_PAGE = "set_page";
export const SET_TOTAL = "set_total";
export const nextPage = () => ({ type: NEXT_PAGE });
export const fetchNextPage = cb => async(nextPage(), cb);
export const prevPage = () => ({ type: PREV_PAGE });
export const fetchPrevPage = cb => async(prevPage(), cb);
export const setPage = page => ({ type: SET_PAGE, value: page });
export const fetchPage = (page, cb) => async(setPage(page), cb);
export const setTotal = total => ({ type: SET_TOTAL, value: total });

export const TOGGLE_YOUGOV_ONLY = "yougov_only";
export const TOGGLE_NO_LISTFUND = "no_listfund";
export const toggleYouGovOnly = on_or_off => ({
  type: TOGGLE_YOUGOV_ONLY,
  value: on_or_off
});
export const toggleNoListfund = on_or_off => ({
  type: TOGGLE_NO_LISTFUND,
  value: on_or_off
});

// export const SET_BY_STATE = "set_by_state";
// export const setByState = a_state => ({ type: SET_BY_STATE, value: a_state });

export const getOneAd = (ad_id, url = `${URL_ROOT}/fbpac-api/ads`) => {
  if (!ad_id) return () => null;

  let path = `${url}/${ad_id}`;
  return dispatch => {
    dispatch(requestingOneAd(ad_id));
    return fetch(path, { method: "GET", credentials: "include" })
      .then(res => res.json())
      .then(ad => {
        dispatch(receiveOneAd(ad));
      });
  };
};

export const getStatesAndDistricts = () => {
  let path = `${URL_ROOT}/fbpac-api/states_and_districts`;
  return (dispatch, getState) => {
    let state = getState();

    if (state.lang) {
      path = path + `?lang=${state.lang}`;
    }
    dispatch(requestingStatesAndDistricts());
    return fetch(path, {
      method: "GET",
      credentials: "include",
      redirect: "follow"
    })
      .then(resp => {
        if (resp.redirected === true) {
          window.location.href = `${URL_ROOT}/fbpac-api/partners/sign_in`;
          return null;
        }
        return resp.json();
      })
      .then(resp => {
        dispatch(receiveStatesAndDistricts(resp));
      });
  };
};

export const getGroupedAttrs = (
  groupingKind = "advertiser",
  recent = "by",
  root_url = `${URL_ROOT}/fbpac-api/ads`
) => {
  let path = `${root_url}/${recent}_${groupingKind + "s"}`;
  return (dispatch, getState) => {
    let state = getState();
    if (state.lang) {
      path = path + `?lang=${state.lang}`;
    }
    dispatch(requestingRecentGroupedAttr());
    return (
      fetch(path, {
        method: "GET",
        credentials: "include",
        redirect: "follow" // in case we get redirected to the login page.
      })
        .then(resp => {
          if (resp.redirected === true) {
            window.location.href = `${URL_ROOT}/fbpac-api/partners/sign_in`;
            return null;
          }
          return resp.json();
        })
        // .then(res => res.json())
        .then(resp => {
          dispatch(receiveRecentGroupedAttr(resp));
        })
    );
  };
};

export const getAdminSummary = (root_url = `${URL_ROOT}/fbpac-api/ads`) => {
  let path = `${root_url}/summarize`;
  return (dispatch, getState) => {
    let state = getState();
    if (state.lang) {
      path = path + `?lang=${state.lang}`;
    }
    dispatch(requestingAdminSummary());
    return (
      fetch(path, {
        method: "GET",
        credentials: "include",
        redirect: "follow" // in case we get redirected to the login page.
      })
        .then(resp => {
          if (resp.redirected === true) {
            window.location.href = `${URL_ROOT}/fbpac-api/partners/sign_in`;
            return null;
          }
          return resp.json();
        })
        // .then(res => res.json())
        .then(resp => {
          dispatch(receivedAdminSummary(resp));
        })
    );
  };
};

export const getHomepageSummary = (root_url = `${URL_ROOT}/fbpac-api/ads`) => {
  let path = `${root_url}/homepage_stats`;
  return (dispatch, getState) => {
    // only support English
    let state = getState();
    if (state.lang) {
      path = path + "?lang=en-US";
    }
    dispatch(requestingHomepageSummary());
    return fetch(path)
      .then(resp => resp.json())
      .then(resp => {
        dispatch(receivedHomepageSummary(resp));
      });
  };
};

export const getAdsByState = (url = `${URL_ROOT}/fbpac-api/ads/by_state`) => {
  return (dispatch, getState) => {
    let state = getState();

    let params = new URLSearchParams();

    if (state.pagination && state.pagination.page) {
      params.set("page", state.pagination.page);
    }

    let query = params.toString().length > 0 ? `?${params.toString()}` : "";
    let new_url = `${"/facebook-ads/admin/by_state/" + state.by_state}${query}`;
    if (location.search !== query) {
      // this history.push is just for when the state got changed  via dropdowns/searches
      // and then we got ads back
      // and then we changed the URL to match
      // we skip the history.push if location.search === query
      // which is true when we got here via a <Link>
      // mutating history OUTSIDE of react-router gets things very confused and you end up with dumb URLs.
      history.push({ search: query }, "", new_url);
    }

    params.set("state", state.by_state); // this is after the history.push because we have states not in the search params but in the path.
    let path = `${url}?${params.toString()}`;

    return fetch(path, { method: "GET", credentials: "include" })
      .then(res => res.json())
      .then(ads => {
        dispatch(
          batch(
            newAds(ads.ads),
            // newEntities(ads.entities),
            // newAdvertisers(ads.advertisers),
            // newTargets(ads.targets),
            setTotal(ads.total),
            setPage(parseInt(params.get("page"), 0) || 0)
          )
        );
      });
  };
};
export const getAds = (url = `${URL_ROOT}/fbpac-api/ads`) => {
  return (dispatch, getState) => {
    let state = getState();

    let params = new URLSearchParams();
    if (state.persona) {
      /* this fork here is the equivalent of serialize() in utils.js */
      url = `${URL_ROOT}/fbpac-api/ads/persona`;
      if (state.persona.age) {
        params.set("age_bucket", state.persona.age);
      }
      if (state.persona.location) {
        params.set("location_bucket", state.persona.location);
      }
      if (state.persona.politics) {
        params.set("politics_bucket", state.persona.politics);
      }
      if (state.persona.gender) {
        params.set("gender", state.persona.gender);
      }
      // if (state.persona.name) {
      //   params.set("persona", state.persona.name);
      // }
      if (state.pagination && state.pagination.page) {
        params.set("page", state.pagination.page);
      }
    } else {
      params = serialize(state);
    }

    let path = `${url}?${params.toString()}`;

    let query = params.toString().length > 0 ? `?${params.toString()}` : "";
    let new_url = `${
      /* in admin, fix the URL if we've gotten somewhere weird */
      location.pathname.indexOf("/admin/")
        ? "/facebook-ads/admin/"
        : location.pathname
    }${query}`;
    if (location.search !== query) {
      // this history.push is just for when the state got changed  via dropdowns/searches
      // and then we got ads back
      // and then we changed the URL to match
      // we skip the history.push if location.search === query
      // which is true when we got here via a <Link>
      // mutating history OUTSIDE of react-router gets things very confused and you end up with dumb URLs.
      history.push({ search: query }, "", new_url);
    }
    return fetch(path, { method: "GET", credentials: "include" })
      .then(res => res.json())
      .then(ads => {
        if (state.persona) {
          dispatch(
            batch(
              newAds(ads.ads),
              // newEntities(ads.entities),
              // newAdvertisers(ads.advertisers),
              // newTargets(ads.targets),
              setTotal(ads.total),
              setPage(parseInt(params.get("page"), 0) || 0)
            )
          );
        } else {
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
        }
      });
  };
};

// Admin actions
export const HIDE_AD = "hide_ad";
export const hideAd = ad => ({
  type: HIDE_AD,
  id: ad.id
});

export const suppressAd = ad => {
  return dispatch => {
    dispatch(hideAd(ad));
    return fetch(`${URL_ROOT}/fbpac-api/ads/${ad.id}/suppress`, {
      method: "PUT",
      body: ad.id,
      credentials: "include"
    }).then(resp => {
      if (!resp.ok) {
        window.location = `${URL_ROOT}/fbpac-api/partners/sign_in`;
      }
    });
  };
};
