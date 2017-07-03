const path = require('path');
const webpack = require('webpack');
const outdir = path.resolve(__dirname, 'dist');
const ENV = process.env.NODE_ENV || 'development';

module.exports = [{
  entry: "./src/content.js",
  output: {
    filename: "content.js",
    path: outdir
  },
  resolve: {
    modules: ["src", "node_modules"]
  },
  devtool: 'source-map'
},{
  context: path.resolve(__dirname, "src"),
  entry: "./popup.jsx",
  output: {
    filename: "popup.js",
    path: outdir
  },
  resolve: {
    extensions: ['.jsx', '.js'],
    modules: ["src", "node_modules"],
    alias: {
      react: 'preact-compat',
      'react-dom': 'preact-compat'
    }
  },
  module: {
    rules: [{
      test: /\.jsx?$/,
      enforce: 'pre',
      use: 'source-map-loader'
    },{
      test: /\.jsx?$/,
      exclude: /node_modules/,
      use: 'babel-loader'
    }]
  },
  plugins: [
    new webpack.NoEmitOnErrorsPlugin(),
    new webpack.DefinePlugin({
      'process.env.NODE_ENV': JSON.stringify(ENV)
    })
  ],
  devtool: 'source-map'
}];
