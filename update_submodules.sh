#!/usr/bin/env bash

git pull --recurse-submodules
git submodule update --remote --recursive
