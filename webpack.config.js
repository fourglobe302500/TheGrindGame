// Note this only includes basic configuration for development mode.
// For a more comprehensive configuration check:
// https://github.com/fable-compiler/webpack-config-template

const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const CopyWebPackPlugin = require("copy-webpack-plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const path = require("path");

const resolve = (filePath) =>
  path.isAbsolute(filePath) ? filePath : path.join(__dirname, filePath);

const CONFIG = {
  // The tags to include the generated JS and CSS will be automatically injected in the HTML template
  // See https://github.com/jantimon/html-webpack-plugin
  indexHtmlTemplate: "./src/Web/index.html",
  fsharpEntry: "./src/Web/output/App.js",
  outputDir: "./deploy/public",
  assetsDir: "./src/Web/public",
  devServerPort: 8080,
};

module.exports = (env, arg) => {
  const mode = arg.mode ?? "development";

  const config = CONFIG;
  const isProduction = mode === "production";

  return {
    entry: {
      app: resolve(config.fsharpEntry),
    },
    output: {
      path: resolve(config.outputDir),
      filename: isProduction ? "[name].[contenthash].js" : "[name].js",
    },
    mode: mode,
    plugins: [
      isProduction &&
        new MiniCssExtractPlugin({
          filename: "style.[name].[contenthash].css",
        }),
      isProduction &&
        new CopyWebPackPlugin({
          patterns: [{ from: resolve(config.assetsDir) }],
        }),

      new HtmlWebpackPlugin({
        filename: "index.html",
        template: resolve(config.indexHtmlTemplate),
      }),
    ].filter(Boolean),
    devServer: {
      static: [
        {
          directory: resolve(config.outputDir),
        },
        {
          directory: resolve(config.assetsDir),
        },
      ],
      host: "0.0.0.0",
      port: config.devServerPort,
      hot: true,
      historyApiFallback: true,
    },
    module: {
      rules: [
        {
          test: /\.fs(x|proj)?$/,
          use: {
            loader: "fable-loader",
            options: {
              babel: {
                presets: [
                  [
                    "@babel/preset-env",
                    {
                      targets: "> 0.25%, not dead",
                      modules: false,
                      // This adds polyfills when needed. Requires core-js dependency.
                      // See https://babeljs.io/docs/en/babel-preset-env#usebuiltins
                      useBuiltIns: "usage",
                      corejs: 3,
                    },
                  ],
                ],
              },
            },
          },
        },
        {
          test: /\.js$/,
          exclude: /node_modules/,
          use: {
            loader: "babel-loader",
            options: {
              presets: [
                [
                  "@babel/preset-env",
                  {
                    targets: "> 0.25%, not dead",
                    modules: false,
                    // This adds polyfills when needed. Requires core-js dependency.
                    // See https://babeljs.io/docs/en/babel-preset-env#usebuiltins
                    useBuiltIns: "usage",
                    corejs: 3,
                  },
                ],
              ],
            },
          },
        },
        {
          test: /\.(sass|scss|css)$/,
          use: [
            isProduction ? MiniCssExtractPlugin.loader : "style-loader",
            "css-loader",
            {
              loader: "sass-loader",
              options: { implementation: require("sass") },
            },
          ],
        },
        {
          test: /\.(png|jpg|jpeg|gif|svg|woff|woff2|ttf|eot)(\?.*)?$/,
          use: ["file-loader"],
        },
        {
          test: /\.js$/,
          enforce: "pre",
          use: ["source-map-loader"],
        },
      ],
    },
  };
};
