module.exports = {
  "env": {
    "browser": true,
    "commonjs": true,
    "node": true,
    "es6": true,
    "webextensions": true
  },
  "globals":{
    "chrome": true
  },
  "extends": [
    "eslint:recommended",
    "plugin:react/all"
  ],
  "parserOptions": {
    "ecmaFeatures": {
      "experimentalObjectRestSpread": true,
      "jsx": true
    },
    "sourceType": "module"
  },
  "plugins": [
    "react"
  ],
  "rules": {
    "no-console":0,
    "indent": ["error", 2],
    "react/jsx-indent": ["error", 2],
    "linebreak-style": [
      "error",
      "unix"
    ],
    "semi": [
      "error",
      "always"
    ],
    "react/no-multi-comp":0,
    "react/jsx-max-props-per-line":0,
    "react/forbid-foreign-prop-types":0,
    "react/jsx-indent-props": ["error", 2]
  },
  "settings":{
    "react":{
      "pragma": "h"
    }
  }
};
