import React from "react";
import { StaticRouter } from "react-router-dom";
import Enzyme, { mount } from "enzyme";
import Adapter from "enzyme-adapter-react-16";
import { AdminAdUnconnected } from "../../../src/components/admin/ad.jsx";

Enzyme.configure({ adapter: new Adapter() });

function setup(ad = false) {
  const props = {
    ad: ad || { id: "fake", suppressed: false },
    onSuppressClick: jest.fn()
  };

  const enzymeWrapper = mount(
    <StaticRouter context={{}}>
      {/* this StaticRouteris necessary so that the <Link> works.*/}
      <AdminAdUnconnected {...props} />
    </StaticRouter>
  );

  return {
    props,
    enzymeWrapper
  };
}

describe("components", () => {
  describe("AdminAdUnconnected", () => {
    it("should render self and subcomponents", () => {
      const { enzymeWrapper } = setup();
      expect(enzymeWrapper.find("div").hasClass("ad")).toBe(true);

      expect(
        enzymeWrapper
          .find("a")
          .getDOMNode()
          .getAttribute("href")
      ).toBe("/facebook-ads/admin/ads/fake");
    });

    it("should call the onSuppressClick method when the button is clicked", () => {
      const { enzymeWrapper, props } = setup();
      const input = enzymeWrapper.find("button");
      input.simulate("click");
      expect(props.onSuppressClick.mock.calls).toHaveLength(1);
    });

    it("should not have the Suppress button shown if the ad is already suppressed", () => {
      const { enzymeWrapper } = setup({
        id: "fake",
        suppressed: true
      });
      expect(enzymeWrapper.find("button").exists()).toEqual(false);
    });
  });
});
