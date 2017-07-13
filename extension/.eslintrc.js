module.exports = {
  "parser": "babel-eslint",
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
    "plugin:react/recommended"
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
    "react/jsx-indent-props": ["error", 2],
    "react/prop-types": 0,
    "eqeqeq": 1
  },
  "settings":{
    "react":{
      "pragma": "h"
    }
  }
};
