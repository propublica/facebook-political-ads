import React from "react";
import Enzyme, { mount } from "enzyme";
import Adapter from "enzyme-adapter-react-16";
import { FrontEnd } from "../../src/components/frontend.jsx";
import { MemoryRouter } from "react-router-dom";
Enzyme.configure({ adapter: new Adapter() });

function setup(path) {
  const props = {};
  const enzymeWrapper = mount(
    <MemoryRouter initialEntries={[path]} initialIndex={0}>
      <FrontEnd {...props} />
    </MemoryRouter>
  );

  return {
    props,
    enzymeWrapper
  };
}
describe("components", () => {
  describe("FrontEnd", () => {
    it("should render itself", () => {
      const { enzymeWrapper } = setup({
        ads: []
      });
      expect(enzymeWrapper.find("#app")).toHaveLength(1);
    });
  });
});
