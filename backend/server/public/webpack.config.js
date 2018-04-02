const path = require("path");
const webpack = require("webpack");
const ENV =
  (process.env.NODE_ENV === "dev" || process.env.NODE_ENV) === "development"
    ? "development"
    : "production";
const ExtractTextPlugin = require("extract-text-webpack-plugin");

const extractSass = new ExtractTextPlugin({
  filename: "styles.css",
  allChunks: true
});

const extractAdminSass = new ExtractTextPlugin({
  filename: "admin.css",
  allChunks: true
});

const plugins = () =>
  [
    new webpack.DefinePlugin({
      "process.env.NODE_ENV": JSON.stringify(ENV)
    }),
    ENV === "production"
      ? new webpack.optimize.ModuleConcatenationPlugin()
      : null,
    ENV === "production" ? new webpack.optimize.UglifyJsPlugin() : null,
    new webpack.NoEmitOnErrorsPlugin()
  ].filter(Boolean);

module.exports = [
  {
    entry: "index.jsx",
    context: path.resolve(__dirname, "src"),
    resolve: {
      extensions: [".jsx", ".js"],
      modules: ["src", "node_modules"]
    },
    output: {
      filename: "index.js",
      path: path.resolve(__dirname, "dist")
    },
    module: {
      rules: [
        {
          test: /\.jsx$/,
          use: ["source-map-loader"],
          enforce: "pre"
        },
        {
          test: /\.jsx?$/,
          exclude: /node_modules/,
          use: "babel-loader?cacheDirectory"
        }
      ]
    },
    plugins: plugins(),
    devtool: "source-map"
  },
  {
    entry: "admin.jsx",
    context: path.resolve(__dirname, "src"),
    resolve: {
      extensions: [".jsx", ".js"],
      modules: ["src", "node_modules"]
    },
    output: {
      filename: "admin.js",
      path: path.resolve(__dirname, "dist")
    },
    module: {
      rules: [
        {
          test: /\.jsx$/,
          use: ["source-map-loader"],
          enforce: "pre"
        },
        {
          test: /\.jsx?$/,
          exclude: /node_modules/,
          use: "babel-loader?cacheDirectory"
        }
      ]
    },
    plugins: plugins(),
    devtool: "source-map"
  },
  {
    entry: "beacons.js",
    context: path.resolve(__dirname, "src"),
    output: {
      filename: "beacons.js",
      path: path.resolve(__dirname, "dist")
    },
    resolve: {
      modules: ["src"]
    },
    plugins: plugins(),
    devtool: "source-map"
  },
  {
    entry: "main.scss",
    context: path.resolve(__dirname, "css"),
    resolve: {
      extensions: [".scss"],
      modules: ["css", "images", "fonts"]
    },
    output: {
      filename: "styles.css",
      path: path.resolve(__dirname, "dist")
    },
    module: {
      rules: [
        {
          test: /\.scss$/,
          exclude: ["admin.scss"],
          use: extractSass.extract({
            use: [
              {
                loader: "css-loader",
                options: {
                  sourceMap: true
                }
              },
              {
                loader: "sass-loader",
                options: {
                  sourceMap: true
                }
              }
            ]
          })
        },
        {
          test: /\.(woff2|woff|svg|png)$/,
          use: {
            loader: "url-loader"
          }
        }
      ]
    },
    plugins: [extractSass].filter(Boolean),
    devtool: "source-map"
  },
  {
    entry: "admin.scss",
    context: path.resolve(__dirname, "css"),
    resolve: {
      extensions: [".scss"],
      modules: ["css"]
    },
    output: {
      filename: "admin.css",
      path: path.resolve(__dirname, "dist")
    },
    module: {
      rules: [
        {
          test: /admin\.scss$/,
          use: extractAdminSass.extract({
            use: [
              {
                loader: "css-loader",
                options: {
                  sourceMap: true
                }
              },
              {
                loader: "sass-loader",
                options: {
                  sourceMap: true
                }
              }
            ]
          })
        },
        {
          test: /\.(woff2|woff|svg|png)$/,
          use: {
            loader: "url-loader"
          }
        }
      ]
    },
    plugins: [extractAdminSass].filter(Boolean),
    devtool: "source-map"
  }
];
