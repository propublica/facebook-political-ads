import * as actions from "../src/actions.js";
import configureMockStore from "redux-mock-store";
import thunk from "redux-thunk";
import fetchMock from "fetch-mock";

const ads = JSON.parse(require("fs").readFileSync(__dirname + "/ads.json"));

describe("actions", () => {
  it("should respond to newAds with an array of ads", () =>
    expect(actions.newAds(ads)).toEqual({
      type: actions.NEW_ADS,
      value: ads
    }));

  it("should respond to receiveOneAd with an ad", () => {
    const ad = ads.ads[0];
    expect(actions.receiveOneAd(ad)).toEqual({
      type: actions.GOT_THAT_AD,
      ad
    });
  });

  it("should respond to requestingOneAd with an id", () => {
    const ad_id = ads.ads[0].id;
    expect(actions.requestingOneAd(ad_id)).toEqual({
      type: actions.REQUESTING_ONE_AD,
      ad_id
    });
  });

  it("should respond to setLang with the language", () =>
    expect(actions.setLang("en-US")).toEqual({
      type: actions.SET_LANG,
      value: "en-US"
    }));

  it("should respond to newSearch with the query", () =>
    expect(actions.newSearch("Trump")).toEqual({
      type: actions.NEW_SEARCH,
      value: "Trump"
    }));

  it("should return an array of actions on batch", () =>
    expect(
      actions.batch(actions.newSearch("Trump"), actions.setLang("en-US"))
    ).toEqual({
      type: actions.BATCH,
      actions: [
        { type: actions.NEW_SEARCH, value: "Trump" },
        { type: actions.SET_LANG, value: "en-US" }
      ]
    }));

  it("should return new entities on newEntities", () =>
    expect(actions.newEntities(ads.entities)).toEqual({
      type: actions.NEW_ENTITIES,
      value: ads.entities
    }));

  it("should return new advertisers on newAdvertisers", () =>
    expect(actions.newAdvertisers(ads.advertisers)).toEqual({
      type: actions.NEW_ADVERTISERS,
      value: ads.advertisers
    }));

  it("should return new targets on newTargets", () =>
    expect(actions.newTargets(ads.targets)).toEqual({
      type: actions.NEW_TARGETS,
      value: ads.targets
    }));

  it("should return an entity on filterEntity", () => {
    const entity = { entity: ads.entities[0].entity };
    expect(actions.filterEntity(entity)).toEqual({
      type: actions.FILTER_ENTITY,
      value: entity
    });
  });

  it("should return an advertiser on filterAdvertiser", () => {
    const advertiser = { advertiser: ads.advertisers[0].advertiser };
    expect(actions.filterAdvertiser(advertiser)).toEqual({
      type: actions.FILTER_ADVERTISER,
      value: advertiser
    });
  });

  it("should return a target on filter_target", () => {
    const target = { target: ads.targets[0].target };
    expect(actions.filterTarget(target)).toEqual({
      type: actions.FILTER_TARGET,
      value: target
    });
  });

  it("should return TOGGLE_TARGET on toggleTarget", () =>
    expect(actions.toggleTarget()).toEqual({ type: actions.TOGGLE_TARGET }));

  it("should return TOGGLE_ADVERTISER on toggleAdvertiser", () =>
    expect(actions.toggleAdvertiser()).toEqual({
      type: actions.TOGGLE_ADVERTISER
    }));

  it("should return TOGGLE_ENTITY on toggleEntity", () =>
    expect(actions.toggleEntity()).toEqual({ type: actions.TOGGLE_ENTITY }));

  it("should return RESET_DROPDOWNS on resetDropdowns", () =>
    expect(actions.resetDropdowns()).toEqual({
      type: actions.RESET_DROPDOWNS
    }));

  it("should return NEXT_PAGE on nextPage", () =>
    expect(actions.nextPage()).toEqual({ type: actions.NEXT_PAGE }));

  it("should return PREV_PAGE on prevPage", () =>
    expect(actions.prevPage()).toEqual({ type: actions.PREV_PAGE }));

  it("should return SET_PAGE and the page on setPage", () =>
    expect(actions.setPage(1)).toEqual({ type: actions.SET_PAGE, value: 1 }));

  it("should return SET_TOTAL and the total on setTotal", () =>
    expect(actions.setTotal(10)).toEqual({
      type: actions.SET_TOTAL,
      value: 10
    }));

  it("should return HIDE_AD and an id on hideAd", () => {
    const id = ads.ads[0].id;
    expect(actions.hideAd(ads.ads[0])).toEqual({
      type: actions.HIDE_AD,
      id
    });
  });

  it("should return LOGIN on login", () =>
    expect(actions.login()).toEqual({ type: actions.LOGIN }));

  it("should return LOGOUT on logout", () =>
    expect(actions.logout()).toEqual({ type: actions.LOGOUT }));
});

const mockStore = configureMockStore([thunk]);

class TextEncoder {}
TextEncoder.prototype.encode = jest.fn(() => "encoded");
global.TextEncoder = TextEncoder;
global.crypto = {
  subtle: {
    importKey: jest.fn(() => Promise.resolve("key")),
    sign: jest.fn(() => Promise.resolve("signed"))
  }
};

describe("async actions", () => {
  afterEach(() => {
    fetchMock.reset();
    fetchMock.restore();
  });

  it("should get an ad on getOneAd", async () => {
    const ad = ads.ads[0];
    fetchMock.getOnce("/facebook-ads/ads/" + ad.id, JSON.stringify(ad));
    const expected = [actions.requestingOneAd(ad.id), actions.receiveOneAd(ad)];
    let store = mockStore({});
    await store.dispatch(actions.getOneAd(ad.id));
    expect(store.getActions()).toEqual(expected);
  });

  it("should suppress an ad", async () => {
    const ad = ads.ads[0];
    const url = "/facebook-ads/admin/ads";
    fetchMock.postOnce(url, "ok");
    const store = mockStore({});
    await store.dispatch(actions.suppressAd(ad));
    expect(store.getActions()).toEqual([actions.hideAd(ad)]);
    store.clearActions();
    fetchMock.postOnce(url, { status: 401 }, { overwriteRoutes: true });
    await store.dispatch(actions.suppressAd(ad));
    expect(store.getActions()).toEqual([actions.hideAd(ad), actions.logout()]);
  });

  it("should allow a user to login", async () => {
    fetchMock.postOnce("/facebook-ads/login", "ok");
    const store = mockStore({});
    await store.dispatch(actions.authorize("test", "test"));
    expect(store.getActions()).toEqual([
      actions.login({
        token:
          "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VybmFtZSI6InRlc3QifQ."
      })
    ]);

    expect(TextEncoder.prototype.encode.mock.calls).toHaveLength(2);
    expect(crypto.subtle.importKey.mock.calls).toHaveLength(1);
    expect(crypto.subtle.sign.mock.calls).toHaveLength(1);
  });
});
