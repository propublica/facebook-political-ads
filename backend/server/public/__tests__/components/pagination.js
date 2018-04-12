import React from "react";
import Enzyme, { mount, shallow } from "enzyme";
import Adapter from "enzyme-adapter-react-16";
import Pagination, {
  PaginationUnconnected
} from "../../src/components/pagination.jsx";
import configureMockStore from "redux-mock-store";
import thunk from "redux-thunk";
import fetchMock from "fetch-mock";
import { NEXT_PAGE, PREV_PAGE, SET_PAGE } from "../../src/actions.js";

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
  describe("PaginationUnconnected", () => {
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
      input.simulate("click", { preventDefault() {} });
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
      input.simulate("click", { preventDefault() {} });
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
      input.simulate("click", { preventDefault() {} });
      expect(props.prev).toHaveBeenCalledTimes(0);
      expect(props.next).toHaveBeenCalledTimes(0);
      expect(props.set).toHaveBeenCalledTimes(1);
    });
  });

  describe("Pagination", () => {
    const mockStore = configureMockStore([thunk]);
    fetchMock.get("/fbpac-api/ads?page=2", "{ \"total\": 4 }");

    let store, wrapper;
    beforeEach(() => {
      const initialState = {
        pagination: {
          page: 2,
          total: 4
        }
      };
      store = mockStore(initialState);
      wrapper = shallow(<Pagination store={store} />).dive();
    });

    it("should set off the PREV_PAGE action when the prev button is clicked", () => {
      wrapper
        .find("a")
        .first()
        .simulate("click", { preventDefault() {} });

      const actions = store.getActions();
      expect(actions).toEqual([expect.objectContaining({ type: PREV_PAGE })]);
    });
    it("should set off the PREV_PAGE action when the next button is clicked", () => {
      wrapper
        .find("a")
        .last()
        .simulate("click", { preventDefault() {} });

      const actions = store.getActions();
      expect(actions).toEqual([expect.objectContaining({ type: NEXT_PAGE })]);
    });
    it("should set off the SET_PAGE action when a page num button is clicked", () => {
      wrapper
        .find("a")
        .at(1)
        .simulate("click", { preventDefault() {} });
      const actions = store.getActions();
      expect(actions).toEqual([expect.objectContaining({ type: SET_PAGE })]);
    });
  });
});
