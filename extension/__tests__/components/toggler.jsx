import React from "react";
import { mount } from "enzyme";
import { Provider } from "react-redux";
import { ToggleType } from "../../src/constants.js";
import { Toggle, Toggler } from "../../src/components/toggler.jsx";
import { toggle } from "../../src/actions.js";

import configureMockStore from "redux-mock-store";

const mockStore = configureMockStore();
describe("toggle", () => {
  it("should render a toggle", () => {
    const politicalStore = mockStore({
      active: ToggleType.RATER,
      language: {}
    });
    const political = mount(
      <Provider store={politicalStore}>
        <Toggle type={ToggleType.RATER} amount={1000} />
      </Provider>
    );
    expect(political).toMatchSnapshot();
    const normalStore = mockStore({
      active: ToggleType.ADS,
      amount: 0,
      language: {}
    });
    const normal = mount(
      <Provider store={normalStore}>
        <Toggle type={ToggleType.ADS} amount={0} />
      </Provider>
    );
    expect(normal).toMatchSnapshot();
    normal
      .find(".toggle")
      .at(0)
      .simulate("click");
    expect(normalStore.getActions()[0]).toEqual(toggle(ToggleType.ADS));
  });
});

describe("toggler", () => {
  it("should render a toggler", () => {
    const politicalStore = mockStore({
      active: ToggleType.RATER,
      language: {},
      ads: [],
      ratings: []
    });
    const political = mount(
      <Provider store={politicalStore}>
        <Toggler />
      </Provider>
    );
    expect(political).toMatchSnapshot();
    const normalStore = mockStore({
      active: ToggleType.ADS,
      language: {},
      ads: [],
      ratings: []
    });
    const normal = mount(
      <Provider store={normalStore}>
        <Toggler />
      </Provider>
    );
    expect(normal).toMatchSnapshot();
  });
});
