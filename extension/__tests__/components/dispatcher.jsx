import React from "react";
import { mount } from "enzyme";
import configureMockStore from "redux-mock-store";
import { Provider } from "react-redux";
import { Dispatcher } from "../../src/components/dispatcher.jsx";
import { acceptTerms } from "../../src/actions.js";

const mockStore = configureMockStore();

describe("dispatcher", () => {
  it("should render onboarding by default", () => {
    fetch.mockResponse("{}");
    const store = mockStore({ language: {}, ads: [], ratings: [] });
    const dispatcher = mount(
      <Provider store={store}>
        <Dispatcher />
      </Provider>
    );
    expect(dispatcher).toMatchSnapshot();
    dispatcher.find("button").simulate("click");
    expect(store.getActions()[0]).toEqual(acceptTerms());
  });

  it("should render thanks when the terms are accepted", () => {
    const dispatcher = mount(
      <Provider
        store={mockStore({ language: {}, ads: [], ratings: [], terms: true })}
      >
        <Dispatcher />
      </Provider>
    );
    expect(dispatcher).toMatchSnapshot();
  });
});
