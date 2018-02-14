import React from "react";
import Enzyme, { mount } from "enzyme";
import Adapter from "enzyme-adapter-react-16";
import Targeting from "../../src/components/targeting.jsx";

Enzyme.configure({ adapter: new Adapter() });

function setup() {
  const props = {
    targeting: "asdfasdf"
  };

  const enzymeWrapper = mount(<Targeting {...props} />);

  return {
    props,
    enzymeWrapper
  };
}

describe("components", () => {
  describe("Targeting", () => {
    it("should render self", () => {
      const { enzymeWrapper } = setup();
      expect(enzymeWrapper.find(".targeting_info").exists()).toBe(true);
    });
  });
});
