#!/usr/bin/env bash

npm install -D --save autoprefixer
npm install -D --save postcss-cli

cd themes/docsy
git submodule update -f --init

cd ../..

hugo
