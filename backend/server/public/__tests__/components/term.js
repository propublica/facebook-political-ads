import React from "react";
import Enzyme, { mount, shallow } from "enzyme";
import Adapter from "enzyme-adapter-react-16";
import Term, { TermUnconnected } from "../../src/components/term.jsx";
import configureMockStore from "redux-mock-store";
import thunk from "redux-thunk";
import fetchMock from "fetch-mock";
import { NEW_SEARCH, SET_PAGE } from "../../src/actions.js";

Enzyme.configure({ adapter: new Adapter() });

function setup(newProps = {}) {
  const props = {
    ...{
      term: "Obama",
      search: "not Obama",
      newSearch: jest.fn()
    },
    ...newProps
  };

  const enzymeWrapper = mount(<TermUnconnected {...props} />);

  return {
    props,
    enzymeWrapper
  };
}

describe("components", () => {
  describe("TermUnconnected", () => {
    it("should render self", () => {
      const { enzymeWrapper } = setup();
      expect(enzymeWrapper.find("button.prefab").exists()).toBe(true);
    });

    it("should have current classname if term == search", () => {
      const { enzymeWrapper } = setup({
        term: "Obama",
        search: "Obama"
      });
      expect(enzymeWrapper.find("button.prefab.current").exists()).toBe(true);
    });
    it("should not have current classname if term != search", () => {
      const { enzymeWrapper } = setup({ term: "Obama", search: "ProPublica" });
      expect(enzymeWrapper.find("button.prefab.current").exists()).toBe(false);
    });
    it("should call newSearch on click", () => {
      const { props, enzymeWrapper } = setup();
      enzymeWrapper.find("button").simulate("click");
      expect(props.newSearch.mock.calls).toHaveLength(1);
    });
  });

  describe("Term", () => {
    const mockStore = configureMockStore([thunk]);
    fetchMock.get("/fbpac-api/ads?", '{ "total": 4 }');
    let store, wrapper;
    beforeEach(() => {
      const initialState = {};
      store = mockStore(initialState);
      wrapper = shallow(<Term store={store} />).dive();
    });

    it("should create a new search again when button is clicked", () => {
      wrapper.find("button").simulate("click");

      const actions = store.getActions();
      expect(actions).toEqual([
        expect.objectContaining({ type: SET_PAGE }),
        expect.objectContaining({ type: NEW_SEARCH })
      ]);
    });
  });
});
