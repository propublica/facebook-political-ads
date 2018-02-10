import * as utils from "../src/utils.js";
import * as actions from "../src/actions.js";
import configureMockStore from "redux-mock-store";

describe("utils", () => {
  test("auth returns the right header", () => {
    expect(utils.auth({ token: "" })).toEqual({ Authorization: "Bearer " });
  });

  test("language returns the right header", () => {
    expect(utils.language("en-US")).toEqual({
      "Accept-Language": "en-US;q=1.0"
    });
  });

  test("headers returns the right headers", () => {
    const auth = utils.auth({ token: "" });
    const lang = utils.language("en-US");
    expect(utils.headers({ token: "" }, "en-US")).toEqual(
      Object.assign({}, auth, lang)
    );
    expect(utils.headers(null, "en-US")).toEqual(lang);
  });

  test("serialize deserialize", () => {
    const mockStore = configureMockStore();
    const store = mockStore({
      search: "Trump",
      pagination: { page: 1 },
      lang: "de-DE",
      advertisers: [
        {
          advertiser: "NRDC+(Natural+Resources+Defense+Council)",
          active: true
        }
      ],
      entities: [{ entity: "Donald Trump", active: true }],
      targets: [{ target: "Age", active: true }]
    });

    const query =
      "search=Trump&advertisers=%5B%22NRDC%2B%28Natural%2BResources%2BDefense%2BCouncil%29%22%5D&targets=%5B%7B%22target%22%3A%22Age%22%7D%5D&entities=%5B%7B%22entity%22%3A%22Donald+Trump%22%7D%5D&page=1&lang=de-DE";

    expect(utils.serialize(store).toString()).toEqual(query);
    history.pushState({}, "", "/facebook-ads/ads?" + query);
    utils.deserialize(store.dispatch);
    const state = store.getState();
    state.entities = state.entities.map(entity => ({ entity: entity.entity }));
    state.targets = state.targets.map(target => ({ target: target.target }));
    state.advertisers = state.advertisers.map(advertiser => ({
      advertiser: advertiser.advertiser
    }));
    expect(store.getActions()[0]).toEqual(
      actions.batch(
        actions.newSearch(state.search),
        actions.newEntities(state.entities),
        actions.filterEntity(state.entities[0]),
        actions.newTargets(state.targets),
        actions.filterTarget(state.targets[0]),
        actions.newAdvertisers(state.advertisers),
        actions.filterAdvertiser(state.advertisers[0]),
        actions.setTotal(10000),
        actions.setPage(state.pagination.page),
        actions.setLang("de-DE")
      )
    );
  });
});
