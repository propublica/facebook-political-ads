import React from "react";
import Enzyme, { mount, shallow } from "enzyme";
import Adapter from "enzyme-adapter-react-16";
import { FrontEnd } from "../../src/components/frontend.jsx";
import { MemoryRouter, Route } from "react-router-dom";
import AdDetail from "components/addetail.jsx";
import AdList from "components/adlist.jsx";

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

    // via https://stackoverflow.com/questions/41531465/how-to-test-react-router-by-enzyme
  });
  describe("FrontEnd (routing)", () => {
    const enzymeWrapper = shallow(
      <MemoryRouter initialEntries={["/facebook-ads/"]}>
        <FrontEnd />
      </MemoryRouter>
    )
      .dive()
      .dive();
    const pathMap = enzymeWrapper.find(Route).reduce((pathMap, route) => {
      const routeProps = route.props();
      pathMap[routeProps.path] = routeProps.component;
      return pathMap;
    }, {});

    it("should render the Ads dashboard for route /", () => {
      expect(pathMap["/facebook-ads"]).toBe(AdList);
    });
    it("should render AdDetail for routes with an ad ID", () => {
      expect(pathMap["/facebook-ads/ad/:ad_id"]).toBe(AdDetail);
    });
  });
});
