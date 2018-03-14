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
      getGroupedAttrs: jest.fn()
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
  });
});
