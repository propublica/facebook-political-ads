import React from "react";
import Enzyme, { mount } from "enzyme";
import Adapter from "enzyme-adapter-react-16";
import { TermUnconnected } from "../../src/components/term.jsx";

Enzyme.configure({ adapter: new Adapter() });

function setup(newProps = {}) {
  const props = {
    ...{
      term: "Obama",
      search: "not Obama",
      newSearch: jest.fn()
    },
    ...newProps
  };

  const enzymeWrapper = mount(<TermUnconnected {...props} />);

  return {
    props,
    enzymeWrapper
  };
}

describe("components", () => {
  describe("Ad", () => {
    it("should render self", () => {
      const { enzymeWrapper } = setup();
      expect(enzymeWrapper.find("button.prefab").exists()).toBe(true);
    });

    it("should have current classname if term == search", () => {
      const { enzymeWrapper } = setup({
        term: "Obama",
        search: "Obama"
      });
      expect(enzymeWrapper.find("button.prefab.current").exists()).toBe(true);
    });
    it("should not have current classname if term != search", () => {
      const { enzymeWrapper } = setup({ term: "Obama", search: "ProPublica" });
      expect(enzymeWrapper.find("button.prefab.current").exists()).toBe(false);
    });
    it("should call newSearch on click", () => {
      const { props, enzymeWrapper } = setup();
      enzymeWrapper.find("button").simulate("click");
      expect(props.newSearch.mock.calls).toHaveLength(1);
    });
  });
});
