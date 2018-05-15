import React from "react";
import Enzyme, { shallow } from "enzyme";
import Adapter from "enzyme-adapter-react-16";
import { GroupedAttrsUnconnected } from "../../../src/components/admin/groupedattrs.jsx";

Enzyme.configure({ adapter: new Adapter() });

function setup(newProps = {}) {
  const props = {
    ...{
      match: { params: { groupingType: "advertiser" } },
      groupedAttribute: [{ advertiser: "Fake", count: "1134" }],
      getGroupedAttrs: jest.fn(),
      onLoad: jest.fn(),
      setLang: jest.fn()
    },
    ...newProps
  };

  const enzymeWrapper = shallow(<GroupedAttrsUnconnected {...props} />);

  return {
    props,
    enzymeWrapper
  };
}

describe("components", () => {
  describe("GroupedAttrsUnconnected", () => {
    it("should get groupedAttrs from the server", () => {
      const { props } = setup();
      expect(props.getGroupedAttrs.mock.calls).toHaveLength(1);
    });

    it("should render self", () => {
      const { enzymeWrapper } = setup();
      expect(enzymeWrapper.find("#advertisers").exists()).toBe(true);
    });

    it("should render as many Links as there are advertisers in GroupedAttribute", () => {
      const { props, enzymeWrapper } = setup();
      expect(enzymeWrapper.find("Link")).toHaveLength(
        props.groupedAttribute.length
      );
    });

    it("should render the right URL format for advertisers", () => {
      const { enzymeWrapper } = setup();
      expect(enzymeWrapper.find("Link").props().to).toEqual(
        "/facebook-ads/admin/ads?advertisers=%5B\"Fake\"%5D"
      );
    });
    it("should render the right URL format for targets", () => {
      const props = {
        match: { params: { groupingType: "target" } },
        groupedAttribute: [{ target: "List", count: "1134" }],
        getGroupedAttrs: jest.fn(),
        onLoad: jest.fn(),
        setLang: jest.fn()
      };
      const enzymeWrapper = shallow(<GroupedAttrsUnconnected {...props} />);
      expect(enzymeWrapper.find("Link").props().to).toEqual(
        "/facebook-ads/admin/ads?targets=%5B%7B\"target\"%3A\"List\"%7D%5D"
      );
    });

    it("should render the right URL format for List/Like target segments", () => {
      const props = {
        match: { params: { groupingType: "segment" } },
        groupedAttribute: [{ segment: "List → ", count: "1134" }],
        getGroupedAttrs: jest.fn(),
        onLoad: jest.fn(),
        setLang: jest.fn()
      };
      const enzymeWrapper = shallow(<GroupedAttrsUnconnected {...props} />);
      expect(enzymeWrapper.find("Link").props().to).toEqual(
        "/facebook-ads/admin/ads?targets=%5B%7B\"target\"%3A\"List\"%7D%5D"
      );
    });
    it("should render the right URL format for two-pronged target segments", () => {
      const props = {
        match: { params: { groupingType: "segment" } },
        groupedAttribute: [{ segment: "Agency → Experian", count: "1134" }],
        getGroupedAttrs: jest.fn(),
        onLoad: jest.fn(),
        setLang: jest.fn()
      };
      const enzymeWrapper = shallow(<GroupedAttrsUnconnected {...props} />);
      expect(enzymeWrapper.find("Link").props().to).toEqual(
        "/facebook-ads/admin/ads?targets=%5B%7B\"target\"%3A\"Agency\",\"segment\"%3A\"Experian\"%7D%5D"
      );
    });
  });
});
