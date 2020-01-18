#!/usr/bin/env bash

set -e

npm install -D --save autoprefixer
npm install -D --save postcss-cli

pushd themes/docsy
git submodule update -f --init

popd

hugo
