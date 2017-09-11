const path = require('path');
const webpack = require('webpack');
const ENV = process.env.NODE_ENV || 'development';

module.exports = [{
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
    })
  ],
  devtool: 'source-map'
}];
