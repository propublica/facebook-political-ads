import * as reducers from "../src/reducers.js";
import * as actions from "../src/actions.js";
import { ToggleType, RatingType } from "../src/constants.js";

const ads = JSON.parse(require("fs").readFileSync(__dirname + "/ads.json"));

describe("thanks", () => {
  it("should say thanks sometimes", () => {
    // find a reasonable chance that we will get a thanks :)
    let count = 1;
    let cumP = 1;
    while (1 - cumP < 0.99999) {
      cumP *= 1 / (2 * Math.sqrt(count));
      count++;
    }
    let message;
    for (let i = 0; i <= count; i++) {
      if ((message = reducers.thanks(null, actions.sayThanks(1)))) break;
    }
    expect(message).not.toBeNull();
  });

  it("should only respond to SAY_THANKS", () =>
    expect(reducers.thanks(null, actions.setLanguage("en"))).toBeNull());
});

describe("active", () => {
  it("should return RATER by default", () =>
    expect(reducers.active(undefined, actions.setLanguage("en"))).toBe(
      ToggleType.RATER
    ));

  it("should toggle a tab", () =>
    expect(
      reducers.active(ToggleType.RATER, actions.toggle(ToggleType.ADS))
    ).toBe(ToggleType.ADS));
});

describe("terms", () => {
  it("should return false by default", () =>
    expect(reducers.terms(undefined, actions.setLanguage("en"))).toBe(false));
  it("should allow a user to accept the terms", () =>
    expect(reducers.terms(undefined, actions.acceptTerms())).toBe(true));
});

describe("ratings_count", () => {
  it("should be zero by default", () =>
    expect(reducers.ratings_count(undefined, actions.setLanguage("en"))).toBe(
      0
    ));
  it("should increment", () =>
    expect(
      reducers.ratings_count(
        0,
        actions.updateRating(ads.ads[0].id, RatingType.POLITICAL)
      )
    ).toBe(1));
});

describe("language", () => {
  it("should respond with the browser's locale by default", () =>
    expect(reducers.language(undefined, actions.acceptTerms())).toBe(
      reducers.browserLocale
    ));

  it("should set the language", () =>
    expect(reducers.language(undefined, actions.setLanguage("en"))).toEqual({
      country: reducers.browserLocale.country,
      language: "en"
    }));

  it("should set the country", () =>
    expect(reducers.language(undefined, actions.setCountry("US"))).toEqual({
      language: reducers.browserLocale.language,
      country: "US"
    }));
});

describe("ads and ratings", () => {
  it("should only respond to new and update", () => {
    expect(reducers.ads(undefined, actions.setLanguage("en"))).toEqual(
      expect.arrayContaining([])
    );
    expect(reducers.ratings(undefined, actions.setLanguage("en"))).toEqual(
      expect.arrayContaining([])
    );
  });

  it("should respond to new", () => {
    expect(reducers.ads(undefined, actions.newAds(ads.ads))).toEqual(
      expect.arrayContaining(ads.ads)
    );
    expect(reducers.ratings(undefined, actions.newRatings(ads.ads))).toEqual(
      expect.arrayContaining(ads.ads)
    );
  });

  it("should respond to update", () => {
    const old = [Object.assign({}, ads.ads[0]), ads.ads[1]];
    const expected = [
      Object.assign({}, { rating: RatingType.POLITICAL }, ads.ads[0]),
      ads.ads[1]
    ];
    expect(
      reducers.ads(old, actions.updateAd(ads.ads[0].id, RatingType.POLITICAL))
    ).toEqual(expect.arrayContaining(expected));
    expect(
      reducers.ratings(
        old,
        actions.updateRating(ads.ads[0].id, RatingType.POLITICAL)
      )
    ).toEqual(expect.arrayContaining(expected));
  });
});
