import React from "react";
import Enzyme, { shallow } from "enzyme";
import Adapter from "enzyme-adapter-react-16";
import { AdsUnconnected } from "../src/components/admin/ads.jsx";
Enzyme.configure({ adapter: new Adapter() });

function setup({ ads }) {
  const props = {
    ads
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
      const { enzymeWrapper } = setup({ ads: [] });
      expect(enzymeWrapper.find("Connect(AdminAdUnconnected)")).toHaveLength(0);
    });
    it("should not render suppressed ads", () => {
      const { enzymeWrapper } = setup({
        ads: [{ suppressed: false }, { suppressed: true }]
      });
      expect(enzymeWrapper.find("Connect(AdminAdUnconnected)")).toHaveLength(1);
    });
    it("should render as many ads as are in props", () => {
      const { enzymeWrapper } = setup({
        ads: [{ suppressed: false }, { suppressed: false }]
      });
      expect(enzymeWrapper.find("Connect(AdminAdUnconnected)")).toHaveLength(2);
    });
    // TODO: this should also test the subscribe() stuff, but hopefully we'll factor that out soon.
  });
});
