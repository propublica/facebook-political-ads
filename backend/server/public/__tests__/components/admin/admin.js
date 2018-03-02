import React from "react";
import { MemoryRouter, StaticRouter, Route } from "react-router-dom";
import Enzyme, { shallow } from "enzyme";
import Adapter from "enzyme-adapter-react-16";
import {
  LoggedInApp,
  AdminUnconnected
} from "../../../src/components/admin/admin.jsx";
import { Ads } from "../../../src/components/admin/ads.jsx";
import { AdDetail } from "../../../src/components/admin/addetail.jsx";
import GroupedAttrs from "../../../src/components/admin/groupedattrs.jsx";
import AdminTools from "../../../src/components/admin/tools.jsx";

Enzyme.configure({ adapter: new Adapter() });

function setup(path) {
  const props = {};

  const enzymeWrapper = shallow(
    <MemoryRouter initialEntries={[path]}>
      <LoggedInApp {...props} />
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
  describe("LoggedInApp", () => {
    // via https://stackoverflow.com/questions/41531465/how-to-test-react-router-by-enzyme
    const { enzymeWrapper } = setup("/facebook-ads/admin/");
    const pathMap = enzymeWrapper.find(Route).reduce((pathMap, route) => {
      const routeProps = route.props();
      pathMap[routeProps.path] = routeProps.component;
      return pathMap;
    }, {});

    it("should render AdDetail for routes with an ad ID", () => {
      expect(pathMap["/facebook-ads/admin/ads/1234567890"]).toBe(AdDetail);
    });
    it("should render the Ads dashboard for route /ads/", () => {
      expect(pathMap["/facebook-ads/admin/ads/"]).toBe(Ads);
    });
    it("should render the Ads dashboard for route /", () => {
      expect(pathMap["/facebook-ads/admin/"]).toBe(Ads);
    });
    it("should render the GroupedAttrs page for route /grouped/by_advertiser", () => {
      expect(pathMap["/facebook-ads/admin/grouped/:groupingType"]).toBe(
        GroupedAttrs
      );
    });
    it("should render the Tools page for route /admin/tools", () => {
      expect(pathMap["/facebook-ads/admin/tools"]).toBe(AdminTools);
    });
  });
  describe("AdminUnconnected", () => {
    it("should not render LoggedInApp when credentials is empty ", () => {
      const enzymeWrapper = shallow(
        <StaticRouter context={{}}>
          <AdminUnconnected credentials={{}} />
        </StaticRouter>
      )
        .dive()
        .dive();

      expect(enzymeWrapper.find("Connect(LoginUnconnected)").exists()).toBe(
        true
      );
      expect(enzymeWrapper.find("LoggedInApp").exists()).toBe(false);
    });
    it("should not render LoggedInApp when credentials doesnt have a token ", () => {
      const enzymeWrapper = shallow(
        <StaticRouter context={{}}>
          <AdminUnconnected credentials={{ token: false }} />
        </StaticRouter>
      )
        .dive()
        .dive();
      expect(enzymeWrapper.find("Connect(LoginUnconnected)").exists()).toBe(
        true
      );
      expect(enzymeWrapper.find("LoggedInApp").exists()).toBe(false);
    });
    it("should render LoggedInApp when credentials.token exists ", () => {
      const enzymeWrapper = shallow(
        <StaticRouter context={{}}>
          <AdminUnconnected credentials={{ token: "asfdasdfas" }} />
        </StaticRouter>
      )
        .dive()
        .dive();
      expect(enzymeWrapper.find("Connect(LoginUnconnected)").exists()).toBe(
        false
      );
      expect(enzymeWrapper.find("LoggedInApp").exists()).toBe(true);
    });
  });
});
