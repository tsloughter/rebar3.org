#!/usr/bin/env bash

set -e

npm install

git submodule update --init --recursive

hugo
