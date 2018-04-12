import React from "react";
import Enzyme, { shallow } from "enzyme";
import Adapter from "enzyme-adapter-react-16";
import {
  AdListUnconnected,
  AdListUnrouted
} from "../../src/components/adlist.jsx";
Enzyme.configure({ adapter: new Adapter() });
import configureMockStore from "redux-mock-store";
import thunkMiddleware from "redux-thunk";
import fetchMock from "fetch-mock";

import * as actions from "../../src/actions.js";

function setup({ ads }) {
  const props = {
    ads
  };

  const enzymeWrapper = shallow(<AdListUnconnected {...props} />, {
    disableLifecycleMethods: true
  });

  return {
    props,
    enzymeWrapper
  };
}
describe("components", () => {
  describe("AdListUnconnected", () => {
    it("should render self", () => {
      const { enzymeWrapper } = setup({ ads: [] });
      expect(enzymeWrapper.find("#facebook-pac-browser").exists()).toBe(true);
    });

    it("should not render any ads if it has no ads in props", () => {
      const { enzymeWrapper } = setup({
        ads: []
      });
      expect(enzymeWrapper.find("Ad")).toHaveLength(0);
    });
    it("should render as many ads as are in props", () => {
      const { enzymeWrapper } = setup({
        ads: [{ id: 1 }, { id: 2 }]
      });
      expect(enzymeWrapper.find("Ad")).toHaveLength(2);
    });

    it("should not render pagination if there are zero ads", () => {
      const { enzymeWrapper } = setup({
        ads: []
      });
      expect(enzymeWrapper.find("Connect(PaginationUnconnected)")).toHaveLength(
        0
      );
    });

    it("should render 2 paginations if there are >=1 ads", () => {
      const { enzymeWrapper } = setup({ ads: [{ id: 1 }, { id: 2 }] });
      expect(enzymeWrapper.find("Connect(PaginationUnconnected)")).toHaveLength(
        2
      );
    });

    it("should render at least one Term", () => {
      const { enzymeWrapper } = setup({
        ads: [{ id: 1 }, { id: 2 }]
      });
      expect(enzymeWrapper.find("Connect(TermUnconnected)").exists()).toBe(
        true
      );
    });
  });

  describe("AdListUnrouted", () => {
    const mockStore = configureMockStore([thunkMiddleware]);
    jest.mock("lodash/debounce", () => jest.fn(fn => fn));
    actions.throttledDispatch = jest.fn(fn => fn);

    it("should preventDefault and call throttledDispatch when change occurs", () => {
      const initialState = { ads: [{ id: "1234567890" }], pagination: null };
      const store = mockStore(initialState);
      const wrapper = shallow(<AdListUnrouted store={store} />, {
        disableLifecycleMethods: true
      }).dive();
      const preventDefault = jest.fn();
      const input_value = "asdfasdf";
      wrapper.find("input").simulate("change", {
        target: { value: input_value },
        preventDefault: preventDefault
      });
      expect(actions.throttledDispatch.mock.calls[0][1]).toBe(input_value);
      expect(preventDefault).toHaveBeenCalledTimes(1);
      expect(actions.throttledDispatch).toHaveBeenCalledTimes(1);
    });

    it("should deserialize on mount", () => {
      const initialState = { ads: [{ id: "1234567890" }], pagination: null };
      const store = mockStore(initialState);
      fetchMock.getOnce("/fbpac-api/ads?", JSON.stringify([]));
      shallow(<AdListUnrouted store={store} />, {
        disableLifecycleMethods: false
      }).dive();
      const calledActions = store.getActions();
      expect(calledActions).toContainEqual(
        expect.objectContaining({ type: actions.BATCH })
      );
    });
  });
});
