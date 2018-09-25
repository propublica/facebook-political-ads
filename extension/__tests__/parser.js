import { Parser, states, errors, parser } from "../src/parser.js";
import { FacebookVM } from "./vm.js";

describe("parser", () => {
  it("should find and collect an ad", async () => {
    const resolve = jest.fn();
    const reject = jest.fn();
    const vm = new FacebookVM();
    const ad = document.querySelector(
      "#hyperfeed_story_id_5ac2bbb055fd90225091584 .userContentWrapper"
    );
    const parse = new Parser(ad, resolve, reject);
    parse.tick();
    expect(parse.state).toEqual(states.TIMELINE);
    parse.tick();
    expect(parse.state).toEqual(states.TIMELINE_ID);
    parse.tick();
    expect(parse.state).toEqual(states.MENU);
    expect(parse.toggleId).toEqual("u_jsonp_2_1d");
    parse.tick();
    expect(parse.idFinder.state).toEqual(states.MENU);
    parse.tick();
    expect(parse.state).toEqual(states.TARGETING);
    parse.tick();
    expect(parse.state).toEqual(states.DONE);
    expect(parse.ad.id).toEqual("23842758929270352");
    expect(parse.ad.targeting).toEqual("targeting");
    parse.tick();
    expect(parse.state).toEqual(states.DONE);
    expect(resolve).toHaveBeenCalled();
    expect(reject).not.toHaveBeenCalled();
    expect(parse.states).toEqual([
      states.TIMELINE,
      states.TIMELINE_ID,
      states.MENU,
      [states.INITIAL, states.MENU, states.DONE],
      states.TARGETING,
      states.WAIT_TARGETING,
      states.DONE
    ]);
    const promise = parser(ad);
    const res = await promise;
    expect(res.id).toEqual("23842758929270352");
    expect(res.targeting).toEqual("targeting");
    vm.detach();
  });

  it("should not collect regular posts", () => {
    const resolve = jest.fn();
    const reject = jest.fn();
    const vm = new FacebookVM();
    const ad = document.querySelector(
      "#hyperfeed_story_id_5ac29976b2dfe1504201819 .userContentWrapper"
    );
    const parse = new Parser(ad, resolve, reject);
    parse.tick();
    expect(parse.state).toEqual(states.TIMELINE);
    parse.tick();
    expect(parse.state).toEqual(states.ERROR);
    parse.tick();
    expect(reject).toHaveBeenCalled();
    expect(resolve).not.toHaveBeenCalled();
    expect(parse.states).toEqual([states.TIMELINE, states.ERROR]);
    expect(parse.message).toEqual(errors.NOT_AN_AD);
    vm.detach();
  });

  it("should find and collect a paid for by ad", async () => {
    const resolve = jest.fn();
    const reject = jest.fn();
    const vm = new FacebookVM();
    const ad = document.querySelector(
      "#hyperfeed_story_id_5b073958c2f8d1e41312928 .userContentWrapper"
    );
    const parse = new Parser(ad, resolve, reject);
    parse.tick();
    expect(parse.state).toEqual(states.TIMELINE);
    parse.tick();
    expect(parse.state).toEqual(states.TIMELINE_ID);
    parse.tick();
    expect(parse.state).toEqual(states.MENU);
    expect(parse.toggleId).toEqual("u_fetchstream_4_v");
    parse.tick();
    expect(parse.idFinder.state).toEqual(states.MENU);
    parse.tick();
    expect(parse.state).toEqual(states.TARGETING);
    parse.tick();
    expect(parse.state).toEqual(states.DONE);
    expect(parse.ad.id).toEqual("23842758929270352");
    expect(parse.ad.targeting).toEqual("targeting");
    parse.tick();
    expect(parse.state).toEqual(states.DONE);
    expect(resolve).toHaveBeenCalled();
    expect(reject).not.toHaveBeenCalled();
    expect(parse.states).toEqual([
      states.TIMELINE,
      states.TIMELINE_ID,
      states.MENU,
      [states.INITIAL, states.MENU, states.DONE],
      states.TARGETING,
      // states.WAIT_TARGETING,
      states.DONE
    ]);
    const promise = parser(ad);
    const res = await promise;
    expect(res.id).toEqual("23842758929270352");
    expect(res.targeting).toEqual("targeting");
    vm.detach();
  });

  it("should find and collect a Sisi's ad", async () => {
    const resolve = jest.fn();
    const reject = jest.fn();
    const vm = new FacebookVM();
    const ad = document.querySelector(
      "#hyperfeed_story_id_5b880d5373d714746163772 .userContentWrapper"
    );
    const parse = new Parser(ad, resolve, reject);
    parse.tick();
    expect(parse.state).toEqual(states.TIMELINE);
    parse.tick();
    expect(parse.state).toEqual(states.TIMELINE_ID);
    parse.tick();
    expect(parse.state).toEqual(states.MENU);
    expect(parse.toggleId).toEqual("u_fetchstream_3_7");
    parse.tick();
    expect(parse.idFinder.state).toEqual(states.MENU);
    parse.tick();
    expect(parse.state).toEqual(states.TARGETING);
    parse.tick();
    expect(parse.state).toEqual(states.DONE);
    expect(parse.ad.id).toEqual("23842758929270352");
    expect(parse.ad.targeting).toEqual("targeting");
    parse.tick();
    expect(parse.state).toEqual(states.DONE);
    expect(resolve).toHaveBeenCalled();
    expect(reject).not.toHaveBeenCalled();
    expect(parse.states).toEqual([
      states.TIMELINE,
      states.TIMELINE_ID,
      states.MENU,
      [states.INITIAL, states.MENU, states.DONE],
      states.TARGETING,
      // states.WAIT_TARGETING,
      states.DONE
    ]);
    const promise = parser(ad);
    const res = await promise;
    expect(res.id).toEqual("23842758929270352");
    expect(res.targeting).toEqual("targeting");
    vm.detach();
  });

  it("should find and collect LKW's ad", async () => {
    const resolve = jest.fn();
    const reject = jest.fn();
    const vm = new FacebookVM();
    const ad = document.querySelector(
      "#hyperfeed_story_id_5b85b34ce73376c62775420 .userContentWrapper"
    );
    const parse = new Parser(ad, resolve, reject);
    parse.tick();
    expect(parse.state).toEqual(states.TIMELINE);
    parse.tick();
    expect(parse.state).toEqual(states.TIMELINE_ID);
    parse.tick();
    expect(parse.state).toEqual(states.MENU);
    expect(parse.toggleId).toEqual("u_fetchstream_2_t");
    parse.tick();
    expect(parse.idFinder.state).toEqual(states.MENU);
    parse.tick();
    expect(parse.state).toEqual(states.TARGETING);
    parse.tick();
    expect(parse.state).toEqual(states.DONE);
    expect(parse.ad.id).toEqual("23842758929270352");
    expect(parse.ad.targeting).toEqual("targeting");
    parse.tick();
    expect(parse.state).toEqual(states.DONE);
    expect(resolve).toHaveBeenCalled();
    expect(reject).not.toHaveBeenCalled();
    expect(parse.states).toEqual([
      states.TIMELINE,
      states.TIMELINE_ID,
      states.MENU,
      [states.INITIAL, states.MENU, states.DONE],
      states.TARGETING,
      // states.WAIT_TARGETING,
      states.DONE
    ]);
    const promise = parser(ad);
    const res = await promise;
    expect(res.id).toEqual("23842758929270352");
    expect(res.targeting).toEqual("targeting");
    vm.detach();
  });

  it("should find and collect a sidebar ad", async () => {
    const resolve = jest.fn();
    const reject = jest.fn();
    const vm = new FacebookVM();
    const ad = document.querySelector("#u_ps_jsonp_12_7_0");
    const parse = new Parser(ad, resolve, reject);
    parse.tick();
    expect(parse.state).toEqual(states.SIDEBAR);
    parse.tick();
    expect(parse.state).toEqual(states.SIDEBAR_ID);
    parse.tick();
    expect(parse.state).toEqual(states.MENU);
    expect(parse.toggleId).toEqual("23843142413060458");
    parse.tick();
    expect(parse.idFinder.state).toEqual(states.MENU);
    parse.tick();
    expect(parse.state).toEqual(states.TARGETING);
    parse.tick();
    expect(parse.state).toEqual(states.DONE);
    expect(parse.ad.id).toEqual("23843142413060458");
    expect(parse.ad.targeting).toEqual("targeting");
    parse.tick();
    expect(parse.state).toEqual(states.DONE);
    expect(resolve).toHaveBeenCalled();
    expect(reject).not.toHaveBeenCalled();
    expect(parse.states).toEqual([
      states.SIDEBAR,
      states.SIDEBAR_ID,
      states.MENU,
      [states.INITIAL, states.MENU, states.DONE],
      states.TARGETING,
      states.WAIT_TARGETING,
      states.DONE
    ]);
    vm.detach();
  });

  it("should find and collect a second sidebar ad", async () => {
    const resolve = jest.fn();
    const reject = jest.fn();
    const vm = new FacebookVM();
    const ad = document.querySelector("#u_ps_jsonp_12_7_1");
    const parse = new Parser(ad, resolve, reject);
    parse.tick();
    expect(parse.state).toEqual(states.SIDEBAR);
    parse.tick();
    expect(parse.state).toEqual(states.SIDEBAR_ID);
    parse.tick();
    expect(parse.state).toEqual(states.MENU);
    expect(parse.toggleId).toEqual("23842851023170087");
    parse.tick();
    expect(parse.idFinder.state).toEqual(states.MENU);
    parse.tick();
    expect(parse.state).toEqual(states.TARGETING);
    parse.tick();
    expect(parse.state).toEqual(states.DONE);
    expect(parse.ad.id).toEqual("23842851023170087");
    expect(parse.ad.targeting).toEqual("targeting");
    parse.tick();
    expect(parse.state).toEqual(states.DONE);
    expect(resolve).toHaveBeenCalled();
    expect(reject).not.toHaveBeenCalled();
    expect(parse.states).toEqual([
      states.SIDEBAR,
      states.SIDEBAR_ID,
      states.MENU,
      [states.INITIAL, states.MENU, states.DONE],
      states.TARGETING,
      states.WAIT_TARGETING,
      states.DONE
    ]);
    vm.detach();
  });
});
