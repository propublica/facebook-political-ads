import * as actions from "../src/actions.js";
import { ToggleType, RatingType } from "../src/constants.js";
import configureMockStore from "redux-mock-store";
import thunk from "redux-thunk";
import fetch from "jest-fetch-mock";
global.fetch = fetch;
global.Headers = fetch.Headers;

const ads = JSON.parse(require("fs").readFileSync(__dirname + "/ads.json"));

describe("actions", () => {
  it("should respond to setLanguage with SET_LANGUAGE", () =>
    expect(actions.setLanguage("en")).toEqual({
      type: actions.SET_LANGUAGE,
      language: "en"
    }));

  it("should respond to setCountry with SET_COUNTRY", () =>
    expect(actions.setCountry("US")).toEqual({
      type: actions.SET_COUNTRY,
      country: "US"
    }));

  it("should respond to acceptTerms with ACCEPT_TERMS", () =>
    expect(actions.acceptTerms()).toEqual({
      type: actions.ACCEPT_TERMS
    }));

  it("should respond to toggle with TOGGLE_TAB", () =>
    expect(actions.toggle(ToggleType.RATER)).toEqual({
      type: actions.TOGGLE_TAB,
      value: ToggleType.RATER
    }));

  it("should respond to newAds with NEW_ADS", () =>
    expect(actions.newAds(ads.ads)).toEqual({
      type: actions.NEW_ADS,
      value: ads.ads
    }));

  it("should respond to updateAd with UPDATE_AD", () =>
    expect(actions.updateAd(ads.ads[0].id, RatingType.POLITICAL)).toEqual({
      type: actions.UPDATE_AD,
      id: ads.ads[0].id,
      value: RatingType.POLITICAL
    }));

  it("should respond to updateRating with UPDATE_RATING", () =>
    expect(actions.updateRating(ads.ads[0].id, RatingType.POLITICAL)).toEqual({
      type: actions.UPDATE_RATING,
      id: ads.ads[0].id,
      value: RatingType.POLITICAL
    }));

  it("should respond to sayThanks with SAY_THANKS", () =>
    expect(actions.sayThanks(1)).toEqual({
      type: actions.SAY_THANKS,
      ratings_count: 1
    }));
});

const mockStore = configureMockStore([thunk]);

describe("async actions", () => {
  it("should post updates to our server on rateAd", async () => {
    const { id, html, targeting } = ads.ads[0];
    const ad = { id, html, targeting, political: true };
    const store = mockStore({ language: "en", country: "US" });

    await store.dispatch(
      actions.rateAd(ad, RatingType.POLITICAL, actions.updateRating)
    );
    expect(store.getActions()[1]).toEqual(actions.sayThanks(0));
    expect(store.getActions()[0]).toEqual(
      actions.updateRating(ad.id, RatingType.POLITICAL)
    );
    fetch.resetMocks();
  });
});
