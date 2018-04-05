import * as utils from "../src/utils.js";
import fetch from "jest-fetch-mock";
import { RatingType } from "../src/constants.js";
import sortBy from "lodash/sortby";
import cloneDeep from "lodash/clonedeep";
global.fetch = fetch;
global.Headers = fetch.Headers;

const ads = JSON.parse(require("fs").readFileSync(__dirname + "/ads.json"));
describe("server communication", () => {
  afterEach(() => fetch.resetMocks());

  it("should format a request for sending", () =>
    expect(utils.adForRequest(ads.ads[0])).toEqual({
      id: ads.ads[0].id,
      html: ads.ads[0].html,
      targeting: ads.ads[0].targeting
    }));

  it("should send ads to the server", async () => {
    await utils.sendAds(ads.ads, { country: "US", language: "en" });
    expect(fetch.mock.calls[0]).toEqual([
      "http://localhost:8080/facebook-ads/ads",
      {
        method: "POST",
        mode: "no-cors",
        headers: new fetch.Headers({ "Accept-Language": "en-US;q=1.0" }),
        body: JSON.stringify(ads.ads)
      }
    ]);
  });

  it("should getAds from the server", async () => {
    const cb = jest.fn();
    fetch.mockResponse("{}");
    await utils.getAds({ country: "US", language: "en" }, cb);
    expect(cb.mock.calls).toHaveLength(1);
    expect(fetch.mock.calls[0]).toEqual([
      "http://localhost:8080/facebook-ads/ads",
      {
        method: "GET",
        headers: new fetch.Headers({ "Accept-Language": "en-US;q=1.0" })
      }
    ]);
  });
});

const badge = jest.fn();
global.chrome = {
  browserAction: {
    setBadgeText: badge
  }
};

describe("badges", () => {
  afterEach(() => badge.mockReset());

  it("should update the badge", () => {
    utils.updateBadge(ads.ads);
    expect(badge.mock.calls[0]).toEqual([{ text: "" + ads.ads.length }]);
  });

  it("should report more than 100", () => {
    let bulk = [];
    while (bulk.length <= 100) bulk = bulk.concat(ads.ads);
    utils.updateBadge(bulk);
    expect(badge.mock.calls[0]).toEqual([{ text: "100+" }]);
  });

  it("should not show a badge when everything is rated", () => {
    const ratings = [
      Object.assign({}, ads.ads[0], { rating: RatingType.POLITICAL })
    ];
    utils.updateBadge(ratings);
    expect(badge.mock.calls[0]).toEqual([{ text: "" }]);
  });
});

describe("merge", () => {
  const old = ads.ads.slice(0, 1);
  const newAds = sortBy(
    ads.ads.slice(0, 2),
    a => -1 * Date.parse(a.created_at)
  );
  expect(utils.mergeAds(old, newAds)).toEqual(expect.arrayContaining(newAds));
});

describe("unrated ratings", () => {
  const ratings = cloneDeep(ads.ads);
  ratings[1].rating = RatingType.POLITICAL;
  ratings[0].rating = RatingType.NORMAL;
  expect(utils.getUnratedRatings(ratings)).toEqual(
    expect.arrayContaining(ratings.slice(1, ratings.length))
  );

  expect(utils.countUnratedRatings(ratings)).toEqual(18);
});
