import React from "react";
import { mount } from "enzyme";
import configureMockStore from "redux-mock-store";
import { Provider } from "react-redux";
import { Onboarding } from "../../src/components/onboarding.jsx";
const mockStore = configureMockStore();

describe("onboarding", () => {
  it("should render", () => {
    const onboarding = mount(
      <Provider store={mockStore({ language: {} })}>
        <Onboarding />
      </Provider>
    );
    expect(onboarding).toMatchSnapshot();
  });
});
