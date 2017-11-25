import { memoize } from "lodash";
import { connect } from 'preact-redux';

const messages = ['da', 'de', 'de_CH', 'en', 'it'].reduce((index, lang) => {
  index[lang] = require(`../_locales/${lang}/messages.json`);
  return index;
}, {});

const createFormatter = (...locales) => {
  const index = locales.reduceRight(Object.assign, {});

  const formatter = (key, replacements) => {
    let message = (
      index[key] && index[key].message ||
      `TK(${key})`
    );
    if (replacements) {
      Object.keys(replacements).forEach(replacementKey => {
        message = message.replace(`$${replacementKey}$`, replacements[replacementKey]);
      });
    }
    return message;
  };

  return formatter;
};

const getFormatter = memoize(({language, country}) => createFormatter(
  messages[`${language}_${country}`],
  messages[`${language}`],
  messages.en // always fallback to English
));

export const withI18n = connect(
  ({language}) => ({
    getMessage: getFormatter(language),
    language
  })
);
