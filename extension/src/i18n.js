import memoize from "lodash/memoize";
import { connect } from "react-redux";
import countries from "i18n-iso-countries";
import * as messages from "locales.js";

// active ones get prioritised in the ui (pull downs)
// ISO 3166-1 Alpha-2 (upper case)
export const activeCountries = [
  "DK",
  "DE",
  "CH",
  "US",
  "IT",
  "AU",
  "NL",
  "FI",
  "CA",
  "SE",
  "BE",
  "SV",
  "BR",
  "ES",
  "MX",
  "GE", // georgia (but like tblisi, not like atlanta)
  "IL"
];
// ISO 639-1 (2 characters, lower case)
export const activeLanguages = [
  "da",
  "de",
  "en",
  "it",
  "nl",
  "fi",
  "fr",
  "sv",
  "pt",
  "es",
  "ka", // georgian
  "he"
];

// load country names in our languages
activeLanguages.forEach(lang =>
  countries.registerLocale(require(`i18n-iso-countries/langs/${lang}.json`))
);

export const createFormatter = (...locales) => {
  const index = locales.reduceRight(Object.assign, {});

  const formatter = (key, replacements) => {
    let message = (index[key] && index[key].message) || `TK(${key})`;
    if (replacements) {
      Object.keys(replacements).forEach(replacementKey => {
        message = message.replace(
          `$${replacementKey}$`,
          replacements[replacementKey]
        );
      });
    }
    return message;
  };

  return formatter;
};

export const getFormatter = memoize(({ language, country }) =>
  createFormatter(
    messages[`${language}_${country}`],
    messages[language],
    messages.en // always fallback to English
  )
);

export const withI18n = connect(({ language }) => ({
  getMessage: getFormatter(language),
  language
}));

export const getBrowserLocale = () => {
  const lang = chrome.i18n.getUILanguage()
    ? chrome.i18n.getUILanguage()
    : navigator.languages && navigator.languages.length
      ? navigator.languages[0]
      : navigator.language;
  return {
    language: lang.split("-")[0],
    country: lang.split("-")[1]
  };
};
