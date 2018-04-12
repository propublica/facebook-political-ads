import React from "react";
import { mount } from "enzyme";
import { Provider } from "react-redux";
import configureMockStore from "redux-mock-store";
import { setLanguage, setCountry } from "../../src/actions.js";
import {
  SelectLanguage,
  SelectCountry,
  Language
} from "../../src/components/language.jsx";

const mockStore = configureMockStore();

describe("language", () => {
  it("should render and let folks change their language", () => {
    const store = mockStore({ language: {} });
    const language = mount(
      <Provider store={store}>
        <SelectLanguage />
      </Provider>
    );
    expect(language).toMatchSnapshot();
    language.find("select").simulate("change", { target: { value: "en" } });
    expect(store.getActions()[0]).toEqual(setLanguage("en"));
  });

  it("should render and let folks change their country", () => {
    const store = mockStore({ language: {} });
    const country = mount(
      <Provider store={store}>
        <SelectCountry />
      </Provider>
    );
    expect(country).toMatchSnapshot();
    country.find("select").simulate("change", { target: { value: "US" } });
    expect(store.getActions()[0]).toEqual(setCountry("US"));
  });

  it("should show the terms", () => {
    const store = mockStore({ language: {} });
    const language = mount(
      <Provider store={store}>
        <Language />
      </Provider>
    );
    expect(language).toMatchSnapshot();
  });
});
