import {
  SET_LANG,
  NEW_SEARCH,
  BATCH,
  NEW_ADS,
  HIDE_AD,
  TOGGLE_TARGET,
  TOGGLE_ADVERTISER,
  TOGGLE_ENTITY,
  RESET_DROPDOWNS,
  NEXT_PAGE,
  PREV_PAGE,
  SET_PAGE,
  SET_TOTAL,
  GOT_THAT_AD,
  REQUESTING_ONE_AD,
  GOT_RECENT_GROUPED_ATTR,
  REQUESTING_RECENT_GROUPED_ATTR,
  CHANGE_POLITICAL_PROBABILITY,
  CLEAR_ALL_FILTERS,
  REQUESTING_ADMIN_SUMMARY,
  GOT_ADMIN_SUMMARY,
  REQUESTING_HOMEPAGE_SUMMARY,
  GOT_HOMEPAGE_SUMMARY,
  RECEIVE_STATES_AND_DISTRICTS,
  REQUESTING_STATES_AND_DISTRICTS,
  CLEAR_PERSONA,
  SET_PERSONA,
  SET_PERSONA_FACET,
  SHOW_OLD_SEARCH,
  HIDE_OLD_SEARCH,
  NEW_BY_STATE,
  FILTER_BY_BY_STATE,
  TOGGLE_YOUGOV_ONLY,
  TOGGLE_NO_LISTFUND
} from "./actions.js";

// https://github.com/reactjs/redux/issues/911#issuecomment-149192251
export const enableBatching = reducer => {
  return function batchingReducer(state, action) {
    switch (action.type) {
      case BATCH:
        return action.actions.reduce(batchingReducer, state);
      default:
        return reducer(state, action);
    }
  };
};

export const lang = (state = null, action) => {
  switch (action.type) {
    case SET_LANG:
      return action.value;
    default:
      return state;
  }
};

export const search = (state = null, action) => {
  switch (action.type) {
    case NEW_SEARCH:
      return action.value;
    default:
      return state;
  }
};

export const show_old_search = (state = false, action) => {
  switch (action.type) {
    case SHOW_OLD_SEARCH:
      return true;
    case HIDE_OLD_SEARCH:
      return false;
    default:
      return state;
  }
};

export const persona = (state = null, action) => {
  switch (action.type) {
    case CLEAR_PERSONA:
      return null;
    case SET_PERSONA:
      return action.value;
    case SET_PERSONA_FACET:
      return state ? { ...state, ...action.value } : action.value;
    case NEW_SEARCH:
      return null;
    default:
      return state;
  }
};

export const ads = (state = [], action) => {
  switch (action.type) {
    case NEW_ADS:
      return action.value;
    case HIDE_AD:
      return state.map(ad => {
        if (ad.id === action.id) {
          return { ...ad, suppressed: true };
        }
        return ad;
      });
    default:
      return state;
  }
};

export const ad = (state = {}, action) => {
  switch (action.type) {
    case GOT_THAT_AD:
      return action.ad;
    case REQUESTING_ONE_AD:
      return null;
    default:
      return state;
  }
};

export const groupedAttribute = (state = [], action) => {
  switch (action.type) {
    case GOT_RECENT_GROUPED_ATTR:
      return action.groupedAttrs;
    case REQUESTING_RECENT_GROUPED_ATTR:
      return null;
    default:
      return state;
  }
};

export const summary = (state = {}, action) => {
  switch (action.type) {
    case GOT_ADMIN_SUMMARY:
      return action.summary;
    case REQUESTING_ADMIN_SUMMARY:
      return null;
    default:
      return state;
  }
};

export const homepage_stats = (state = {}, action) => {
  switch (action.type) {
    case GOT_HOMEPAGE_SUMMARY:
      return action.summary;
    case REQUESTING_HOMEPAGE_SUMMARY:
      return null;
    default:
      return state;
  }
};

export const statesAndDistricts = (state = {}, action) => {
  switch (action.type) {
    case RECEIVE_STATES_AND_DISTRICTS:
      return action.statesAndDistricts;
    case REQUESTING_STATES_AND_DISTRICTS:
      return null;
    default:
      return state;
  }
};

export const filters = (state = {}, action) => {
  switch (action.type) {
    case TOGGLE_TARGET:
      return { ...state, target: !state.target };
    case TOGGLE_ADVERTISER:
      return { ...state, advertiser: !state.advertiser };
    case TOGGLE_ENTITY:
      return { ...state, entity: !state.entity };
    case RESET_DROPDOWNS:
      return {};
    default:
      return state;
  }
};

const makeObjectReducer = (plural, singular) => {
  return (state = [], action) => {
    switch (action.type) {
      case `new_${plural}`: {
        const lookup = new Set(
          state.filter(filter => filter.active).map(it => it[singular])
        );
        const filtersWithSegments = state.filter(filter => filter.segment);
        return action.value.map(filter => {
          let oldFilter = filtersWithSegments.find(
            old => old[singular] === filter[singular]
          );
          return singular !== "target" || !oldFilter
            ? {
              ...filter,
              key: filter[singular],
              active: lookup.has(filter[singular])
            }
            : {
              ...filter,
              key: filter[singular],
              active: lookup.has(filter[singular]),
              segment: oldFilter.segment
            };
        });
      }
      case `filter_${singular}`:
        return state.map(filter => {
          if (filter[singular] === action.value[singular]) {
            return {
              ...filter,
              active: !filter.active
            };
          } else {
            return filter;
          }
        });
      case CLEAR_ALL_FILTERS:
        return [];
      default:
        return state;
    }
  };
};

const makeArrayReducer = plural => {
  return (state = [], action) => {
    switch (action.type) {
      case `new_${plural}`: {
        return action.value;
      }
      case `filter_${plural}`:
        return state;
      case CLEAR_ALL_FILTERS:
        return [];
      default:
        return state;
    }
  };
};

export const entities = makeObjectReducer("entities", "entity");
export const advertisers = makeObjectReducer("advertisers", "advertiser");
export const targets = makeObjectReducer("targets", "target");
export const states = makeArrayReducer("states");
export const parties = makeArrayReducer("parties");
export const districts = makeArrayReducer("districts");

const makeBooleanReducer = action_name => (state = null, action) => {
  switch (action.type) {
    case action_name:
      return action.value;
    default:
      return state;
  }
};

export const yougov_only = makeBooleanReducer(TOGGLE_YOUGOV_ONLY);
export const no_listfund = makeBooleanReducer(TOGGLE_NO_LISTFUND);

export const by_state = (state = null, action) => {
  switch (action.type) {
    case NEW_BY_STATE:
      return action.value;
    case FILTER_BY_BY_STATE:
      return action.value;
    default:
      return state;
  }
};

const PER_PAGE = 20;
const min = state => Math.min(state.page, state.total);
export const pagination = (
  state = { page: 0, total: 0, total_ads: 0 },
  action
) => {
  switch (action.type) {
    case NEXT_PAGE:
      return { ...state, page: min({ ...state, page: state.page + 1 }) };
    case PREV_PAGE:
      return { ...state, page: Math.max(state.page - 1, 0) };
    case SET_PAGE:
      return {
        ...state,
        page: min({ page: action.value, total: state.total })
      };
    case SET_TOTAL: {
      const total_ads = action.value;
      const total = Math.ceil(action.value / PER_PAGE);
      return { total, total_ads, page: 0 };
    }
    default:
      return state;
  }
};

export const politicalProbability = (state = [], action) => {
  switch (action.type) {
    case CHANGE_POLITICAL_PROBABILITY: {
      return action.value;
    }
    default:
      return state;
  }
};
