import * as reducers from "../src/reducers.js";
import * as actions from "../src/actions.js";

const j = () => JSON.parse(require("fs").readFileSync(__dirname + "/ads.json"));

const ads = j().ads;
const suppressed = j().ads;
const resp = j();
suppressed[0].suppressed = true;

describe("batch", () => {
  const reducer = jest.fn();
  const batched = reducers.enableBatching(reducer);

  it("should batch actions", () => {
    batched(
      [],
      actions.batch(actions.setLang("en-US"), actions.newSearch("Trump"))
    );
    expect(reducer.mock.calls).toHaveLength(2);
  });

  it("should respond to regular actions", () => {
    batched([], actions.setLang("en-US"));
    expect(reducer.mock.calls).toHaveLength(3);
  });
});

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

describe("ad", () => {
  it("should respond to GOT_THAT_AD", () =>
    expect(reducers.ad(null, actions.receiveOneAd(ads[0]))).toEqual(ads[0]));

  it("should respond to REQUESTING_ONE_AD", () =>
    expect(reducers.ad(null, actions.requestingOneAd(ads[0]))).toEqual(null));

  it("should only respond to GOT_THAT_AD and REQUESTING_ONE_AD", () =>
    expect(reducers.ad(ads[0], actions.newSearch("Trump"))).toEqual(ads[0]));
});

describe("filters", () => {
  it("should respond to TOGGLE_TARGET", () =>
    expect(reducers.filters({}, actions.toggleTarget())).toEqual({
      target: true
    }));

  it("should respond to TOGGLE_ENTITY", () =>
    expect(reducers.filters({}, actions.toggleEntity())).toEqual({
      entity: true
    }));

  it("should respond to TOGGLE_ADVERTISER", () =>
    expect(reducers.filters({}, actions.toggleAdvertiser())).toEqual({
      advertiser: true
    }));

  it("should reset dropdowns", () =>
    expect(
      reducers.filters(
        { target: true, advertiser: true, entity: true },
        actions.resetDropdowns()
      )
    ).toEqual({}));

  it("should only respond to filter actions", () =>
    expect(reducers.filters(undefined, actions.newSearch("Trump"))).toEqual(
      {}
    ));

  const testFilter = (
    label,
    collection,
    reducer,
    key,
    actionNew,
    actionFilter
  ) => {
    const set = reducer([], actionNew(collection));
    const newColl = collection.map(it => ({
      ...it,
      key: it[key],
      active: false
    }));
    it(`should set ${label}`, () => expect(set).toEqual(newColl));
    const filter = reducer(set, actionFilter(collection[0]));
    const filterOut = collection.map(it => ({
      ...it,
      key: it[key],
      active: it[key] === collection[0][key]
    }));
    it(`should filter ${label}`, () => expect(filter).toEqual(filterOut));
    it(`should ignore non ${label} calls`, () =>
      expect(reducer([], actions.newSearch("Trump"))).toEqual([]));
  };

  testFilter(
    "entities",
    resp.entities,
    reducers.entities,
    "entity",
    actions.newEntities,
    actions.filterEntity
  );

  testFilter(
    "advertisers",
    resp.advertisers,
    reducers.advertisers,
    "advertiser",
    actions.newAdvertisers,
    actions.filterAdvertiser
  );

  testFilter(
    "targets",
    resp.targets,
    reducers.targets,
    "target",
    actions.newTargets,
    actions.filterTarget
  );
});

describe("pagination", () => {
  const initial = reducers.pagination(
    reducers.pagination(undefined, {}),
    actions.setTotal(20)
  );
  it("should set the total", () =>
    expect(initial).toEqual({ page: 0, total: 1 }));

  const inc = it => reducers.pagination(it, actions.nextPage());
  const dec = it => reducers.pagination(it, actions.prevPage());

  it("should increment the page", () =>
    expect(inc(initial)).toEqual({ page: 1, total: 1 }));

  it("should stop at the last page", () =>
    expect(inc(inc(initial))).toEqual({ page: 1, total: 1 }));

  it("should go back one page", () =>
    expect(dec(inc(initial))).toEqual(initial));
  it("should stop at the first page", () =>
    expect(dec(initial)).toEqual(initial));
  it("should set a page", () =>
    expect(reducers.pagination(initial, actions.setPage(20))).toEqual({
      page: 1,
      total: 1
    }));
});
