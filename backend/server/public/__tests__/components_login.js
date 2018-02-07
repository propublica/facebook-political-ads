import React from "react";
import Enzyme, { mount } from "enzyme";
import Adapter from "enzyme-adapter-react-16";
import { LoginUnconnected } from "../src/components/admin/login.jsx";

Enzyme.configure({ adapter: new Adapter() });

function setup() {
  const props = {
    authorize: jest.fn()
  };

  const enzymeWrapper = mount(<LoginUnconnected {...props} />);

  return {
    props,
    enzymeWrapper
  };
}

describe("components", () => {
  describe("LoginUnconnected", () => {
    it("should render self", () => {
      const { enzymeWrapper } = setup();
      expect(enzymeWrapper.find("#login").exists()).toBe(true);
    });

    it("should call the authorize method when the button is clicked", () => {
      const { enzymeWrapper, props } = setup();
      const input = enzymeWrapper.find("#submit");
      input.simulate("submit");
      expect(props.authorize.mock.calls).toHaveLength(1);
    });
  });
});
