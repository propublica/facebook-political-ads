import React from "react";
import Enzyme, { shallow } from "enzyme";
import Adapter from "enzyme-adapter-react-16";
import {
  AdsUnconnected,
  AdsUnrouted
} from "../../../src/components/admin/ads.jsx";
import configureMockStore from "redux-mock-store";
import { NEW_SEARCH } from "../../../src/actions.js";
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
    it("should not render suppressed ads", () => {
      const { enzymeWrapper } = setup({
        ads: [{ id: 1, suppressed: false }, { id: 2, suppressed: true }],
        pagination: null
      });
      expect(enzymeWrapper.find("Connect(AdminAdUnconnected)")).toHaveLength(1);
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
      expect(enzymeWrapper.find("Connect(PaginationUnconnected)")).toHaveLength(
        1
      );
    });
  });

  // describe("AdminAds (lifecycle methods)", () => {
  //   it("should call call deserialize on mount", () => {
  //     const props = {
  //       ads: [],
  //       pagination: null,
  //       deserialize: jest.fn()
  //     };
  //     const mockStore = configureMockStore();
  //     const initialState = { ads: [{ id: "1234567890" }], pagination: null };
  //     let store = mockStore(initialState);
  //     shallow(
  //       <Provider store={store}>
  //         <AdsUnconnected {...props} />
  //       </Provider>
  //     ).dive();
  //     expect(props.deserialize.mock.calls).toHaveLength(1);
  //   });
  // });

  describe("AdminAds", () => {
    const mockStore = configureMockStore();
    let store, wrapper;

    it("should create a new search again when keyUp occurs", () => {
      const initialState = { ads: [{ id: "1234567890" }], pagination: null };
      store = mockStore(initialState);
      wrapper = shallow(<AdsUnrouted store={store} />, {
        disableLifecycleMethods: true
      }).dive();
      wrapper
        .find("input")
        .simulate("keyUp", { target: { value: "asdfa" }, preventDefault() {} });

      const actions = store.getActions();
      setTimeout(
        () =>
          expect(actions).toEqual([
            expect.objectContaining({ type: NEW_SEARCH })
          ]),
        1000 // we wait because this is debounced.
      );
    });
  });
});
