import * as i18n from "../src/i18n.js";

describe("i18n", () => {
  const testFormatter = i18n.createFormatter(
    { greeting: { message: "Hello! $name$" } },
    { greeting: { message: "Hola! $name$" }, goodbye: { message: "Adios!" } }
  );
  it("should give us a formatter", () =>
    expect(testFormatter).toBeInstanceOf(Function));

  it("should do a translation", () =>
    expect(testFormatter("greeting", { name: "Jeff" })).toBe("Hello! Jeff"));

  it("should fallback to the default language", () =>
    expect(testFormatter("goodbye")).toBe("Adios!"));

  it("should not blow up on missing translations", () =>
    expect(testFormatter("salutation")).toBe("TK(salutation)"));

  const realFormatter = i18n.getFormatter({ language: "de", country: "DE" });
  it("should translate german", () =>
    expect(realFormatter("description")).toBe(
      "Sucht nach politischer Werbung auf Facebook"
    ));

  it("should get the browser's locale", () =>
    expect(i18n.getBrowserLocale()).toEqual(
      expect.objectContaining({
        country: expect.any(String),
        language: expect.any(String)
      })
    ));
});
