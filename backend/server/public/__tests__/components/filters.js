import React from "react";
import Enzyme, { mount } from "enzyme";
import Adapter from "enzyme-adapter-react-16";
import { FiltersUnconnected } from "../../src/components/filters.jsx";

Enzyme.configure({ adapter: new Adapter() });

function setup(newProps = {}) {
  const props = {
    ...{
      filters: { entity: "Mueller" },
      entities: [],
      advertisers: [],
      targets: []
    },
    ...newProps
  };

  const enzymeWrapper = mount(<FiltersUnconnected {...props} />);

  return {
    props,
    enzymeWrapper
  };
}

describe("components", () => {
  describe("FiltersUnconnected", () => {
    it("should render self", () => {
      const { enzymeWrapper } = setup();
      expect(enzymeWrapper.find(".filters").exists()).toBe(true);
    });
    it("should render at least one Filter item", () => {
      const { enzymeWrapper } = setup();
      expect(enzymeWrapper.find(".filter").exists()).toBe(true);
    });
  });
});
