import React from "react";
import Enzyme, { shallow } from "enzyme";
import Adapter from "enzyme-adapter-react-16";
import { AdDetailUnconnected } from "../../src/components/addetail.jsx";
Enzyme.configure({ adapter: new Adapter() });

function setup(newProps = {}) {
  const props = {
    ...{
      ad: {},
      match: { params: { ad_id: "1234567890" } },
      getOneAd: jest.fn()
    },
    ...newProps
  };
  const enzymeWrapper = shallow(<AdDetailUnconnected {...props} />);

  return {
    props,
    enzymeWrapper
  };
}
describe("components", () => {
  describe("AdDetailUnconnected", () => {
    it("should get the ad for the ID if props.match (i.e. URL) exists", () => {
      const { props } = setup({
        match: { params: { ad_id: "1234567890" } },
        ad: { id: null }
      });
      expect(props.getOneAd.mock.calls).toHaveLength(1);
    });

    it("should render a Not Found thing when this.props.ad is empty", () => {
      const { enzymeWrapper } = setup({ ad: {} });
      expect(enzymeWrapper.find("h2.error").exists()).toEqual(false);
      expect(enzymeWrapper.find("h2.notfound").exists()).toEqual(true);
      expect(enzymeWrapper.find("#ad").exists()).toEqual(false);
    });

    it("should render a Link to all ads", () => {
      const { enzymeWrapper } = setup();
      expect(enzymeWrapper.find(".all-link").exists()).toEqual(true);
    });
  });
});
