// Based on https://github.com/fable-compiler/webpack-config-template.

var path = require("path");
var webpack = require("webpack");
var htmlWebpackPlugin = require('html-webpack-plugin');
var copyWebpackPlugin = require('copy-webpack-plugin');
var miniCssExtractPlugin = require("mini-css-extract-plugin");

var config = {
    indexHtmlTemplate: "./src/ui/index.html"
    , fsharpEntry: "./src/ui/ui.fsproj"
    , sassEntry: "./src/ui/style/sweepstake-2021-bulma.sass"
    , cssEntry: "./src/ui/style/sweepstake-2021.css"
    , outputDir: "./src/ui/publish"
    , assetsDir: "./src/ui/public"
    , babel: {
        presets: [
            ["@babel/preset-env", {
                "modules": false
                , "useBuiltIns": "usage"
				, corejs: 3
            }]
        ]
    }
}

var isProduction = process.argv.indexOf("-p") >= 0;
var isAzure = process.argv.indexOf("--verbose") >= 0;
console.log("Bundling for " + (isProduction ? (isAzure ? "production (azure)" : "production (local)") : "development") + "...");

// HtmlWebpackPlugin automatically injects <script> or <link> tags for generated bundles.
var commonPlugins = [
    new htmlWebpackPlugin({
        filename: 'index.html'
        , template: resolve(config.indexHtmlTemplate)
    })
];

module.exports = {
    // In development, bundle styles together with the code so they can also trigger hot reloads; in production, put them in a separate CSS file.
    entry: isProduction ? {
        app: [resolve(config.fsharpEntry), resolve(config.sassEntry), resolve(config.cssEntry)]
    } : {
            app: [resolve(config.fsharpEntry)]
            , style: [resolve(config.sassEntry), resolve(config.cssEntry)]
        }
    , output: {
        path: resolve(config.outputDir)
        , filename: isProduction ? '[name].[hash].js' : '[name].js'
    }
    , mode: isProduction ? "production" : "development"
    , devtool: isProduction ? "source-map" : "eval-source-map"
    , optimization: {
        splitChunks: {
            cacheGroups: {
                commons: {
                    test: /node_modules/
                    , name: "vendors"
                    , chunks: "all"
                }
            }
        }
    }
    , plugins: isProduction
        ? commonPlugins.concat([
            new miniCssExtractPlugin({ filename: 'style.css' })
            , new copyWebpackPlugin({ patterns: [{ from: resolve(config.assetsDir) }]})
        ])
        : commonPlugins.concat([
            new webpack.HotModuleReplacementPlugin()
            , new webpack.NamedModulesPlugin()
        ])
    , resolve: {
        symlinks: false
    }
    , devServer: {
        publicPath: "/"
        , contentBase: resolve(config.assetsDir)
        , hot: true
        , inline: true
    }
    , module: {
        rules: [
            {
                test: /\.fs(x|proj)?$/
                , use: {
                    loader: "fable-loader"
                    , options: {
                        babel: config.babel
                        //, define: isProduction ? [ "ACTIVITY", "TICK" ] : [ "DEBUG", "ACTIVITY", "TICK"/*, "HMR"*/ ] // note: enable HMR if required (though this seems to cause ServerHub to reset client state to NotRegistered, which can lead to confusing behaviour)
                        , define: (isProduction ? (isAzure ? [ "AZURE", "TICK" ] : [ "TICK" ]) : [ "DEBUG", "TICK"/*, "HMR"*/ ]) // note: enable HMR if required
                    }
                }
            }
            , {
                test: /\.js$/
                , exclude: /node_modules/
                , use: {
                    loader: 'babel-loader'
                    , options: config.babel
                },
            }
            , {
                test: /\.(sass|scss|css)$/
                , use: [
                    isProduction ? miniCssExtractPlugin.loader : 'style-loader'
                    , 'css-loader'
                    , {
                        loader: 'sass-loader'
                        , options: { implementation: require("sass") }
                    }
                ]
            }
            , {
                test: /\.(png|jpg|jpeg|gif|svg|woff|woff2|ttf|eot)(\?.*)?$/
                , use: ["file-loader"]
            }
        ]
    }
};

function resolve(filePath) {
    return path.isAbsolute(filePath) ? filePath : path.join(__dirname, filePath);
}
