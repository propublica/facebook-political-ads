const path = require('path');
const webpack = require('webpack');
const UglifyJSPlugin = require('uglifyjs-webpack-plugin');
const ENV = 'production';

module.exports = [{
  entry: "index.jsx",
  context: path.resolve(__dirname, "src"),
  resolve: {
    extensions: ['.jsx', '.js'],
    modules: ["src", "node_modules"]
  },
  output: {
    filename: 'index.js',
    path: path.resolve(__dirname, 'dist')
  },
  module: {
    rules: [{
      test: /\.jsx$/,
      use: ["source-map-loader"],
      enforce: "pre"
    },{
      test: /\.jsx?$/,
      exclude: /node_modules/,
      use: 'babel-loader'
    }]
  },
  plugins: [
    new webpack.optimize.ModuleConcatenationPlugin(),
    new webpack.NoEmitOnErrorsPlugin(),
    new webpack.DefinePlugin({
      'process.env.NODE_ENV': JSON.stringify(ENV)
    }),
    new UglifyJSPlugin({
      sourceMap: true
    })
  ],
  devtool: 'source-map'
},{
  entry: "admin.jsx",
  context: path.resolve(__dirname, "src"),
  resolve: {
    extensions: ['.jsx', '.js'],
    modules: ["src", "node_modules"]
  },
  output: {
    filename: 'admin.js',
    path: path.resolve(__dirname, 'dist')
  },
  module: {
    rules: [{
      test: /\.jsx$/,
      use: ["source-map-loader"],
      enforce: "pre"
    },{
      test: /\.jsx?$/,
      exclude: /node_modules/,
      use: 'babel-loader'
    }]
  },
  plugins: [
    new webpack.optimize.ModuleConcatenationPlugin(),
    new webpack.NoEmitOnErrorsPlugin(),
    new webpack.DefinePlugin({
      'process.env.NODE_ENV': JSON.stringify(ENV)
    }),
    new UglifyJSPlugin({
      sourceMap: true
    })
  ],
  devtool: 'source-map'
},{
  entry: "beacons.js",
  context: path.resolve(__dirname, "src"),
  output: {
    filename: 'beacons.js',
    path: path.resolve(__dirname, 'dist')
  },
  resolve: {
    modules: ["src"]
  },
  plugins: [
    new webpack.optimize.ModuleConcatenationPlugin(),
    new webpack.NoEmitOnErrorsPlugin(),
    new UglifyJSPlugin({
      sourceMap: true
    })
  ],
  devtool: 'source-map'
}];
