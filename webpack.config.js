var webpack = require('webpack');
var path = require('path');
var HtmlWebpackPlugin = require( 'html-webpack-plugin' );

module.exports = {
    output: {
        path: path.resolve(__dirname, 'dist/'),
        filename: '[hash].js',
    },

    resolve: {
        modulesDirectories: ['node_modules'],
        extensions: ['', '.js', '.elm']
    },

    module: {
        noParse: /\.elm$/,
        loaders: [
            {
                test: /\.(eot|ttf|woff|woff2|svg)$/,
                loader: 'file-loader'
            },
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                loader: 'elm-hot!elm-webpack?verbose=true&warn=true'
            }
        ]
    },

    plugins: [
        new HtmlWebpackPlugin(
        {
            template: 'src/index.html',
            inject: 'body',
            filename: 'index.html'
        })
    ],

    entry: [
        'webpack-dev-server/client?http://localhost:8080',
        path.join(__dirname, 'src/index.js')
    ],

    devServer: {
        inline: true,
        progress: true
    },
};
