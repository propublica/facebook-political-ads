import * as reducers from "../src/reducers.js";
import * as actions from "../src/actions.js";

const j = () => JSON.parse(require("fs").readFileSync(__dirname + "/ads.json"));

const ads = j().ads;
const suppressed = j().ads;
suppressed[0].suppressed = true;

describe("lang", () => {
  it("should set lang", () =>
    expect(reducers.lang(null, actions.setLang("en-US"))).toEqual("en-US"));

  it("should only respond to SET_LANG", () =>
    expect(reducers.lang("en-US", actions.newAds([]))).toEqual("en-US"));
});

describe("search", () => {
  it("should set search", () =>
    expect(reducers.search(null, actions.newSearch("Trump"))).toEqual("Trump"));

  it("should only respond to NEW_SEARCH", () =>
    expect(reducers.search("Trump", actions.setLang("en-US"))).toEqual(
      "Trump"
    ));
});

describe("ads", () => {
  it("should set new ads", () =>
    expect(reducers.ads([], actions.newAds(ads))).toEqual(ads));

  it("should suppress an ad", () =>
    expect(reducers.ads(ads, actions.hideAd(ads[0]))).toEqual(suppressed));

  it("should only respond to NEW_ADS and HIDE_ADS", () =>
    expect(reducers.ads(ads, actions.newSearch("Trump"))).toEqual(ads));
});

describe("ad", () => {});
