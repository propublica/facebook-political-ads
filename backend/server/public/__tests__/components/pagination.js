import React from "react";
import Enzyme, { mount } from "enzyme";
import Adapter from "enzyme-adapter-react-16";
import { PaginationUnconnected } from "../../src/components/pagination.jsx";

Enzyme.configure({ adapter: new Adapter() });

function setup(newProps = {}) {
  const props = {
    ...{
      page: 1,
      total: 3,
      prev: jest.fn(),
      next: jest.fn(),
      set: jest.fn()
    },
    ...newProps
  };

  const enzymeWrapper = mount(<PaginationUnconnected {...props} />);

  return {
    props,
    enzymeWrapper
  };
}

describe("components", () => {
  describe("Pagination", () => {
    it("should render self", () => {
      const { enzymeWrapper } = setup();
      expect(enzymeWrapper.find("nav.pagination").exists()).toBe(true);
    });

    it("should not render self if no pages", () => {
      const { enzymeWrapper } = setup({ page: 0, total: 0 });
      expect(enzymeWrapper.find("nav.pagination li").exists()).toBe(false);
    });

    it("should render left arrow if we're not on first page", () => {
      const { enzymeWrapper } = setup({ page: 2, total: 10 });
      expect(
        enzymeWrapper
          .find("nav.pagination li")
          .first()
          .text()
      ).toEqual("←");
    });
    it("should render right arrow if we're not on last page", () => {
      const { enzymeWrapper } = setup({ page: 2, total: 10 });
      expect(
        enzymeWrapper
          .find("nav.pagination li")
          .last()
          .text()
      ).toEqual("→");
    });
    it("should not render left arrow if we're on first page", () => {
      const { enzymeWrapper } = setup({ page: 0, total: 10 });
      expect(
        enzymeWrapper
          .find("nav.pagination li")
          .last()
          .text()
      ).not.toEqual("←");
    });
    it("should not render right arrow if we're on last page", () => {
      const { enzymeWrapper } = setup({ page: 9, total: 10 });
      expect(
        enzymeWrapper
          .find("nav.pagination li")
          .last()
          .text()
      ).not.toEqual("→");
    });

    it("should call prev on left arrow click", () => {
      const { enzymeWrapper, props } = setup({ page: 2, total: 10 });
      const input = enzymeWrapper
        .find("nav.pagination li")
        .first()
        .find("a");
      input.simulate("click");
      expect(props.prev).toHaveBeenCalledTimes(1);
      expect(props.next).toHaveBeenCalledTimes(0);
      expect(props.set).toHaveBeenCalledTimes(0);
    });
    it("should call next on right arrow click", () => {
      const { enzymeWrapper, props } = setup({ page: 2, total: 10 });
      const input = enzymeWrapper
        .find("nav.pagination li")
        .last()
        .find("a");
      input.simulate("click");
      expect(props.prev).toHaveBeenCalledTimes(0);
      expect(props.next).toHaveBeenCalledTimes(1);
      expect(props.set).toHaveBeenCalledTimes(0);
    });
    it("should call set on page number click", () => {
      const { enzymeWrapper, props } = setup({ page: 2, total: 10 });
      const input = enzymeWrapper
        .find("nav.pagination li")
        .at(5)
        .find("a");
      input.simulate("click");
      expect(props.prev).toHaveBeenCalledTimes(0);
      expect(props.next).toHaveBeenCalledTimes(0);
      expect(props.set).toHaveBeenCalledTimes(1);
    });
  });
});
