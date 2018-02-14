import React from "react";
import Enzyme, { shallow } from "enzyme";
import Adapter from "enzyme-adapter-react-16";
import { AdsUnconnected } from "../../../src/components/admin/ads.jsx";
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
  describe("AdminAdDetailUnconnected", () => {
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
});
