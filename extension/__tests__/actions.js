import * as actions from "../src/actions.js";

describe("actions", () => {
  it("should respond to setLanguage with SET_LANGUAGE", () =>
    expect(actions.setLanguage("en-US")).toEqual({
      type: actions.SET_LANGUAGE,
      language: "en-US"
    }));
});
