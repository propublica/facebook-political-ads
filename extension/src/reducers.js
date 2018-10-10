import {
  ACCEPT_TERMS,
  SAY_THANKS,
  TOGGLE_TAB,
  UPDATE_RATING,
  SET_LANGUAGE,
  SET_COUNTRY
} from "actions.js";
import { ToggleType, MOST_RECENT_STORIES_UPDATE } from "constants.js";
import { getBrowserLocale } from "i18n.js";
import { mergeAds } from "utils.js";

// Reducers
export const active = (state = ToggleType.RATER, action) => {
  switch (action.type) {
    case TOGGLE_TAB:
      return action.value;
    default:
      return state;
  }
};

export const stories_seen = (state = 0, action) => {
  // if the user switches away from the Stories tab, then they've seen the stories so far.
  switch (action.type) {
    case TOGGLE_TAB:
      return action.oldTab === ToggleType.STORIES
        ? MOST_RECENT_STORIES_UPDATE
        : state;
    default:
      return state;
  }
};

export const buildUpdate = type => (state = [], action) => {
  switch (action.type) {
    case "new_" + type + "s":
      return mergeAds(state, action.value);
    case "update_" + type:
      return mergeAds(
        state.map(ad => {
          if (ad.id === action.id) {
            return { ...ad, rating: action.value };
          }
          return ad;
        }),
        []
      );
    default:
      return state;
  }
};

export const ads = buildUpdate("ad");
export const ratings = buildUpdate("rating");

export const terms = (state = false, action) => {
  switch (action.type) {
    case ACCEPT_TERMS:
      return true;
    default:
      return state;
  }
};

export const thanks_messages = [
  "thanks1", // "Thanks! 🙂",
  "thanks2", // "Keep it up!",
  "thanks3", // "We couldn't do it without you!",
  "thanks4", // "Thanks for helping out!",
  "thanks5" //  "Way to go! You've categorized $count$ ads so far."
];
export const thanks = (state = null, action) => {
  switch (action.type) {
    case SAY_THANKS: {
      // we thank you frequently at first and exponentially less often later
      // for instance, your first ad, we have a 50% chance of thanking you
      // and your 4th ad a 25% chance and so on.
      let message =
        Math.random() < 1 / (2 * Math.sqrt(action.ratings_count || 1))
          ? thanks_messages[parseInt(Math.random() * thanks_messages.length)]
          : null;
      return message;
    }
    default:
      return state;
  }
};

export const ratings_count = (state = 0, action) => {
  switch (action.type) {
    case UPDATE_RATING:
      return state + 1;
    default:
      return state;
  }
};

export const browserLocale = getBrowserLocale();
export const language = (state = browserLocale, action) => {
  switch (action.type) {
    case SET_LANGUAGE:
      return { ...state, language: action.language };
    case SET_COUNTRY:
      return { ...state, country: action.country };
    default:
      return state;
  }
};
