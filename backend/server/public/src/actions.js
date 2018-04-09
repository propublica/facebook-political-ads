import { headers, serialize } from "utils.js";
import { debounce } from "lodash";
import history from "./history.js";

export const URL_ROOT =
  window.location.href.indexOf("localhost") > -1 ? "http://localhost:3000" : "";

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

const asyncResetPage = action => {
  return (dispatch, getState) => {
    dispatch(setPage(0));
    return async(action)(dispatch, getState);
  };
};

const async = action => {
  return (dispatch, getState) => {
    dispatch(action);
    return getAds()(dispatch, getState);
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
export const newEntities = a(NEW_ENTITIES);
export const newAdvertisers = a(NEW_ADVERTISERS);
export const newTargets = a(NEW_TARGETS);

export const FILTER_ENTITY = "filter_entity";
export const FILTER_ADVERTISER = "filter_advertiser";
export const FILTER_TARGET = "filter_target";
export const filterEntity = a(FILTER_ENTITY);
export const filterAdvertiser = a(FILTER_ADVERTISER);
export const filterTarget = a(FILTER_TARGET);
export const fetchEntity = e => asyncResetPage(filterEntity(e));
export const fetchAdvertiser = a => asyncResetPage(filterAdvertiser(a));
export const fetchTarget = t => asyncResetPage(filterTarget(t));

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

export const NEXT_PAGE = "next_page";
export const PREV_PAGE = "prev_page";
export const SET_PAGE = "set_page";
export const SET_TOTAL = "set_total";
export const nextPage = () => ({ type: NEXT_PAGE });
export const fetchNextPage = () => async(nextPage());
export const prevPage = () => ({ type: PREV_PAGE });
export const fetchPrevPage = () => async(prevPage());
export const setPage = page => ({ type: SET_PAGE, value: page });
export const fetchPage = page => async(setPage(page));
export const setTotal = total => ({ type: SET_TOTAL, value: total });

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

export const RECENT = "recent";
export const getGroupedAttrs = (
  groupingKind = "advertiser",
  recent = null,
  root_url = `${URL_ROOT}/fbpac-api/ads`
) => {
  let path = `${root_url}/${
    recent === RECENT ? "this_month_" : "by_"
  }${groupingKind + "s"}`;
  return dispatch => {
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

export const getAds = (url = `${URL_ROOT}/fbpac-api/ads`) => {
  return (dispatch, getState) => {
    let state = getState();
    const params = serialize(state);
    let path = `${url}?${params.toString()}`;

    let query = params.toString().length > 0 ? `?${params.toString()}` : "";
    history.push({ search: query }, "", `${location.pathname}${query}`);
    return fetch(path, { method: "GET", credentials: "include" })
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

export const LOGIN = "login";
export const login = credentials => ({
  type: LOGIN,
  value: credentials
});

export const LOGOUT = "logout";
export const logout = () => ({
  type: LOGOUT
});

export const b64 = thing =>
  btoa(thing)
    .replace(/\+/g, "-")
    .replace(/\//g, "_")
    .replace(/=/g, "");

export const createJWT = (username, password) => {
  const encoder = new TextEncoder();
  const header = {
    alg: "HS256",
    typ: "JWT"
  };
  const payload = {
    username
  };
  const base = `${b64(JSON.stringify(header))}.${b64(JSON.stringify(payload))}`;
  const encoded = encoder.encode(base);
  return crypto.subtle
    .importKey(
      "raw",
      encoder.encode(password),
      { name: "HMAC", hash: { name: "SHA-256" } },
      false,
      ["sign"]
    )
    .then(key => crypto.subtle.sign({ name: "HMAC" }, key, encoded))
    .then(signature => ({
      token: `${base}.${b64(
        String.fromCharCode.apply(null, new Uint8Array(signature))
      )}`
    }));
};

export const authorize = (username, password) => {
  // create jwt
  return dispatch =>
    createJWT(username, password).then(token => {
      return fetch("/facebook-ads/login", {
        method: "POST",
        headers: headers(token)
      }).then(resp => {
        if (resp.ok) {
          dispatch(login(token));
        }
      });
    });
};
