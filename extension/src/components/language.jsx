import React from "react";
import { connect } from "react-redux";
import langs from "langs";
import { setLanguage, setCountry } from "actions.js";
import { withI18n, activeCountries, activeLanguages } from "i18n.js";
import countries from "i18n-iso-countries";
import { getBrowserLocale } from "i18n.js";

export const SelectLanguageUnconnected = ({ language, onChange }) => {
  const allLangs = langs.all();

  const createOption = lang => (
    <option key={lang["1"]} value={lang["1"]}>
      {lang["name"]} / {lang["local"]}
    </option>
  );

  return (
    <select value={language} onChange={onChange}>
      {allLangs.filter(l => activeLanguages.includes(l["1"])).map(createOption)}
      <option disabled>──────────</option>
      {allLangs
        .filter(l => !activeLanguages.includes(l["1"]))
        .map(createOption)}
    </select>
  );
};
const selectLanguageDispatchToProps = dispatch => ({
  onChange: e => {
    dispatch(setLanguage(e.target.value));
  }
});
export const SelectLanguage = connect(
  state => state.language,
  selectLanguageDispatchToProps
)(SelectLanguageUnconnected);

export const SelectCountryUnconnected = ({ language, country, onChange }) => {
  const lang = countries.langs().includes(language) ? language : "en";
  const names = countries.getNames(lang);
  const keys = Object.keys(names);

  const createOption = alpha2 => (
    <option key={alpha2} value={alpha2}>
      {names[alpha2]}
    </option>
  );

  return (
    <select value={country} onChange={onChange}>
      {keys.filter(k => activeCountries.includes(k)).map(createOption)}
      <option disabled>──────────</option>
      {keys.filter(k => !activeCountries.includes(k)).map(createOption)}
    </select>
  );
};
const selectCountryDispatchToProps = dispatch => ({
  onChange: e => {
    dispatch(setCountry(e.target.value));
  }
});
export const SelectCountry = connect(
  state => state.language,
  selectCountryDispatchToProps
)(SelectCountryUnconnected);

const browserLocale = getBrowserLocale();
export const Language = withI18n(({ getMessage, language }) => (
  <div id="language">
    <h2>{getMessage("language_settings")}</h2>
    <p
      dangerouslySetInnerHTML={{
        __html: getMessage("you_speak", {
          language:
            langs.where("1", browserLocale.language).local ||
            "Unknown Language",
          country:
            countries.getName(browserLocale.country, language.language) ||
            countries.getName(browserLocale.country, "en") ||
            "Unknown Country"
        })
      }}
    />
    <p>{getMessage("language_instructions")}</p>
    <p>
      <label>
        {getMessage("language")}
        <br />
        <SelectLanguage />
      </label>
      <br />
      <label>
        {getMessage("country")}
        <br />
        <SelectCountry />
      </label>
    </p>
  </div>
));
