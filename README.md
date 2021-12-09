## Rebar3.org

[![Netlify Status](https://api.netlify.com/api/v1/badges/7f9a24ea-990d-40de-88c5-a4c64d90c155/deploy-status)](https://app.netlify.com/sites/rebar3/deploys)

[Hugo](https://gohugo.io/) site for [Rebar3](https://rebar3.org) documentation.

### Initial Setup

Download and install [Hugo](https://github.com/gohugoio/hugo/releases) extended version `0.52.0` or newer. Extended means the [binary release](https://github.com/gohugoio/hugo/releases) with `_extended` in the name.

The site uses the [docsy](https://www.docsy.dev/) theme and requires [PostCSS](https://www.docsy.dev/docs/getting-started/#install-postcss) to update the site's CSS resources and create the final assets.

To install `PostCSS` and fetch the `docsy` theme as a submodule run:

```shell
$ npm install -D --save autoprefixer
$ npm install -D --save postcss-cli

$ cd themes/docsy
$ git submodule update -f --init
```

## Development

The `hugo` server will run a local web server on port `1313` and automatically reload changes:

```shell
$ hugo server
```
