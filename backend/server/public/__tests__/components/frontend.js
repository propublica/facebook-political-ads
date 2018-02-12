import React from "react";
import Enzyme, { shallow } from "enzyme";
import Adapter from "enzyme-adapter-react-16";
import { FrontEnd } from "../../src/components/frontend.jsx";
import { MemoryRouter } from "react-router-dom";
Enzyme.configure({ adapter: new Adapter() });
import { go } from "i18n.js";

function setup(path) {
  const props = {};
  const enzymeWrapper = shallow(
    <MemoryRouter initialEntries={[path]}>
      <FrontEnd {...props} />
    </MemoryRouter>
  )
    .dive()
    .dive();

  return {
    props,
    enzymeWrapper
  };
}
describe("components", () => {
  describe("FrontEnd", () => {
    it("should render itself", () => {
      go(() => {
        const { enzymeWrapper } = setup({
          ads: []
        });
        expect(enzymeWrapper.find("#app")).toHaveLength(1);
      });
    });

    it("should render AdDetail on a detail route", () => {
      go(() => {
        const { enzymeWrapper } = setup("facebook-ads/ad/1234567890");
        expect(enzymeWrapper.find("Connect(AdDetail)")).toHaveLength(1);
      });
    });

    it("should render AdList on index routes", () => {
      go(() => {
        const { enzymeWrapper } = setup("facebook-ads/");
        expect(enzymeWrapper.find("Connect(AdList)")).toHaveLength(1);
      });
    });
  });
});
