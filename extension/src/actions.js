import { sendAds, adForRequest } from "utils.js";
import { RatingType } from "constants.js";

// Action Types
export const ACCEPT_TERMS = "accept_terms";
export const TOGGLE_TAB = "toggle_tab";
export const NEW_ADS = "new_ads";
export const NEW_RATINGS = "new_ratings";
export const UPDATE_AD = "update_ad";
export const UPDATE_RATING = "update_rating";
export const SET_LANGUAGE = "set_language";
export const SET_COUNTRY = "set_country";
export const SAY_THANKS = "say_thanks";

// Actions
export const setLanguage = language => ({ type: SET_LANGUAGE, language });
export const setCountry = country => ({ type: SET_COUNTRY, country });
export const acceptTerms = () => ({ type: ACCEPT_TERMS });
export const toggle = (newActiveTab, oldActiveTab) => ({
  type: TOGGLE_TAB,
  value: newActiveTab,
  oldTab: oldActiveTab
});

export const newAds = ads => ({
  type: NEW_ADS,
  value: ads
});

export const newRatings = ratings => ({
  type: NEW_RATINGS,
  value: ratings
});

export const updateAd = (id, rating) => ({
  type: UPDATE_AD,
  id: id,
  value: rating
});

export const updateRating = (id, rating) => ({
  type: UPDATE_RATING,
  id: id,
  value: rating
});

export const sayThanks = ratings_count => ({
  type: SAY_THANKS,
  ratings_count
});

export const rateAd = (ad, rating, update) => {
  return (dispatch, getState) => {
    let body = {
      ...adForRequest(ad),
      political: rating === RatingType.POLITICAL
    };
    dispatch(update(ad.id, rating));
    dispatch(sayThanks(getState().ratings_count || 0));
    let cb = () => ({});
    return sendAds([body], getState().language).then(cb, cb);
  };
};
