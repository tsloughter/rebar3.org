#!/usr/bin/env bash

set -e

npm install

cd themes/docsy
git submodule update -f --init

cd ../..

hugo
