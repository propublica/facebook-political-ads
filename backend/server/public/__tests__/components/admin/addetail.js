import React from "react";
import Enzyme, { shallow } from "enzyme";
import Adapter from "enzyme-adapter-react-16";
import { AdDetailUnconnected } from "../../../src/components/admin/addetail.jsx";

Enzyme.configure({ adapter: new Adapter() });

function setup(newProps = {}) {
  const props = {
    ...{
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
  describe("AdminAdDetailUnconnected", () => {
    it("should get the ad for the ID from props.match (i.e. URL)", () => {
      const { props } = setup();
      expect(props.getOneAd.mock.calls).toHaveLength(1);
    });
    it("should render a Not Found thing when this.props.ad is empty", () => {
      const { enzymeWrapper } = setup({ ad: {} });
      expect(enzymeWrapper.find("h2.error").exists()).toEqual(false);
      expect(enzymeWrapper.find("h2.notfound").exists()).toEqual(true);
      expect(enzymeWrapper.find("#ad").exists()).toEqual(false);
    });
    it("should render an Ad when this.props.ad exists and isn't empty", () => {
      const { enzymeWrapper } = setup({ ad: { id: 123456789 } });
      expect(enzymeWrapper.find("h2.error").exists()).toEqual(false);
      expect(enzymeWrapper.find("h2.notfound").exists()).toEqual(false);
      expect(enzymeWrapper.find("#ad").exists()).toEqual(true);
    });
    it("should give an error if match is null", () => {
      const { enzymeWrapper } = setup({ match: null });
      expect(enzymeWrapper.find("h2.error").exists()).toEqual(true);
      expect(enzymeWrapper.find("h2.notfound").exists()).toEqual(false);
      expect(enzymeWrapper.find("#ad").exists()).toEqual(false);
    });
  });
});
