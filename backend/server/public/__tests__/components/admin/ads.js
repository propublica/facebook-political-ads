import React from "react";
import Enzyme, { shallow } from "enzyme";
import Adapter from "enzyme-adapter-react-16";
import {
  AdsUnconnected,
  AdsUnrouted
} from "../../../src/components/admin/ads.jsx";
import configureMockStore from "redux-mock-store";
import thunkMiddleware from "redux-thunk";
import fetchMock from "fetch-mock";

import * as actions from "../../../src/actions.js";

Enzyme.configure({ adapter: new Adapter() });

function setup({ ads, pagination }) {
  const props = {
    ads,
    pagination
  };

  const enzymeWrapper = shallow(<AdsUnconnected {...props} />, {
    disableLifecycleMethods: true
  });

  return {
    props,
    enzymeWrapper
  };
}

describe("components", () => {
  describe("AdminAdsUnconnected", () => {
    it("should not render any ads if it has no ads in props", () => {
      const { enzymeWrapper } = setup({
        ads: [],
        pagination: null
      });
      expect(enzymeWrapper.find("Connect(AdminAdUnconnected)")).toHaveLength(0);
    });
    it("should render as many ads as are in props", () => {
      const { enzymeWrapper } = setup({
        ads: [{ id: 1, suppressed: false }, { id: 2, suppressed: false }],
        pagination: null
      });
      expect(enzymeWrapper.find("Connect(AdminAdUnconnected)")).toHaveLength(2);
    });
    it("should not render pagination if its not supplied", () => {
      const { enzymeWrapper } = setup({
        ads: [{ id: 1, suppressed: false }, { id: 2, suppressed: false }],
        pagination: null
      });
      expect(enzymeWrapper.find("Connect(PaginationUnconnected)")).toHaveLength(
        0
      );
    });

    it("should render pagination if its supplied", () => {
      const { enzymeWrapper } = setup({
        ads: [{ id: 1, suppressed: false }, { id: 2, suppressed: false }],
        pagination: { prev: null, next: null, set: null, pages: 1, total: 10 }
      });
      expect(
        enzymeWrapper.find("Connect(PaginationUnconnected)").length
      ).toBeGreaterThanOrEqual(1);
    });
    it("should call deserialize on mount", () => {
      const props = { deserialize: jest.fn(), ads: [] };
      shallow(<AdsUnconnected {...props} />);
      expect(props.deserialize.mock.calls).toHaveLength(1);
    });
  });

  describe("AdminAds", () => {
    const mockStore = configureMockStore([thunkMiddleware]);
    actions.throttledDispatch = jest.fn(fn => fn);

    it("should preventDefault and call throttledDispatch again when keyUp occurs", () => {
      const initialState = { ads: [{ id: "1234567890" }], pagination: null };
      const store = mockStore(initialState);
      const preventDefault = jest.fn();
      const wrapper = shallow(<AdsUnrouted store={store} />, {
        disableLifecycleMethods: true
      }).dive();
      const input_value = "asdfasfd";
      wrapper.find("input").simulate("keyUp", {
        target: { value: input_value },
        preventDefault: preventDefault
      });
      expect(preventDefault).toHaveBeenCalledTimes(1);
      expect(actions.throttledDispatch).toHaveBeenCalledTimes(1);
      expect(actions.throttledDispatch.mock.calls[0][1]).toBe(input_value);
    });

    it("should not render suppressed ads", () => {
      const initialState = {
        ads: [{ id: 1, suppressed: false }, { id: 2, suppressed: true }],
        pagination: null
      };
      const store = mockStore(initialState);
      const wrapper = shallow(<AdsUnrouted store={store} />, {
        disableLifecycleMethods: true
      }).dive();
      expect(wrapper.find("Connect(AdminAdUnconnected)")).toHaveLength(1);
    });

    it("should deserialize on mount", () => {
      const initialState = { ads: [{ id: "1234567890" }], pagination: null };
      const store = mockStore(initialState);
      fetchMock.getOnce("/fbpac-api/ads?", JSON.stringify([]));
      const wrapper = shallow(<AdsUnrouted store={store} />, {
        disableLifecycleMethods: false
      }).dive();
      const calledActions = store.getActions();
      expect(calledActions).toContainEqual(
        expect.objectContaining({ type: actions.BATCH })
      );
    });
  });
});
