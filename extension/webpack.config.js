var path = require('path');

const outdir = path.resolve(__dirname, 'dist');

module.exports = ["popup.js", "content.js"].map((it) => {
  return {
    entry: "./src/" + it,
    output: {
      filename: it,
      path: outdir
    },
    resolve: {
      modules: ["src", "node_modules"]
    }
  };
});
