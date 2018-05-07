var path = require("path");
var webpack = require("webpack");
var fableUtils = require("fable-utils");
var extractTextPlugin = require('extract-text-webpack-plugin');

const extractSass = new extractTextPlugin({
  filename: './style/custom-bulma.css',
  allChunks: true
});
const extractCss = new extractTextPlugin({
  filename: './style/sweepstake-2018.css',
  allChunks: true
});

function resolve(filePath) {
  return path.join(__dirname, filePath)
}

var babelOptions = fableUtils.resolveBabelOptions({
  presets: [
    [ "env" , {
      "targets": {
        "browsers": [ "last 2 versions" ]
      } ,
      "modules": false
    } ]
  ],
  plugins: [ "transform-runtime" ]
});

var isProduction = process.argv.indexOf("-p") >= 0;
var isLocal = process.argv.indexOf("--verbose") >= 0;
console.log("Bundling for " + (isProduction ? (isLocal ? "production (local)" : "production (azure)") : "development") + "...");

module.exports = {
  devtool: "source-map",
  entry: [ resolve('./ui.fsproj') , '../sass/custom-bulma.sass' , '../css/sweepstake-2018.css' ],
  output: {
    path: resolve('./public'),
    publicPath: "/public",
    filename: "./js/ui.js"
  },
  resolve: {
    modules: [ resolve("../../node_modules/") ]
  },
  devServer: {
    hot: true,
    inline: true
  },
  module: {
    rules: [
      {
        test: /\.fs(x|proj)?$/,
        use: {
          loader: "fable-loader",
          options: {
            babel: babelOptions,
            define: (isProduction ? (isLocal ? [ "TICK" ] : [ "AZURE" , "TICK" ]) : [ "DEBUG" , "HMR" /*, "TICK"*/ ])
          }
        }
      },
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader',
          options: babelOptions
        },
      },
      {
        test: /\.(sass|scss)$/,
        loader: extractSass.extract([ 'css-loader' , 'sass-loader' ])
      },
      {
        test: /\.css$/,
        /* Note: Use 'css-loader?-minimize' to disable minification if required, e.g. '.table.default td' behaves okay if only specifies 'border-bottom-color' - but
                 if specifies 'border-[top|left|bottom|right]-color', minified version just has 'border' (which appears to override 'border-bottom-width' and thus
                 prevent table lines from appearing). */
        use: extractCss.extract('css-loader')
	  }
    ]
  },
  plugins: isProduction ? [ extractSass, extractCss ] : [ extractSass , extractCss , new webpack.HotModuleReplacementPlugin() , new webpack.NamedModulesPlugin() ]
};
