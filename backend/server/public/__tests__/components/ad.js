import React from "react";
import Enzyme, { mount } from "enzyme";
import Adapter from "enzyme-adapter-react-16";
import Ad from "../../src/components/ad.jsx";

Enzyme.configure({ adapter: new Adapter() });

function setup(ad = false) {
  const props = {
    ad: ad || { id: "fake" }
  };

  const enzymeWrapper = mount(<Ad {...props} />);

  return {
    props,
    enzymeWrapper
  };
}

describe("components", () => {
  describe("Ad", () => {
    it("should render self and subcomponents", () => {
      const { enzymeWrapper } = setup();
      expect(enzymeWrapper.find("div.ad").exists()).toBe(true);
    });

    it("should render targeting if present", () => {
      const { enzymeWrapper } = setup({
        id: "fake",
        targeting: { asdf: "asdfx" }
      });
      expect(enzymeWrapper.find(".targeting").exists()).toBe(true);
    });
    it("should not render targeting if absent", () => {
      const { enzymeWrapper } = setup();
      expect(enzymeWrapper.find(".targeting").exists()).toBe(true);
    });
  });
});
