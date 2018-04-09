import React from "react";
import { mount } from "enzyme";
import configureMockStore from "redux-mock-store";
import { Provider } from "react-redux";
import { Thanks } from "../../src/components/thanks.jsx";

const mockStore = configureMockStore();

describe("thanks", () => {
  it("should render", () => {
    const store = mockStore({
      language: { country: "US", language: "en" },
      ads: [],
      ratings: [],
      thanks: "thanks1"
    });
    const thanks = mount(
      <Provider store={store}>
        <Thanks />
      </Provider>
    );
    expect(thanks).toMatchSnapshot();
  });
});
