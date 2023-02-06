#!/usr/bin/env bash

# Update submodules
git pull --recurse-submodules
git submodule update --remote --recursive
