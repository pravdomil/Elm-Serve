#!/usr/bin/env bash

# Stop if any command fails.
set -e

# Stop on unset variables.
set -u

# Be in project root.
cd "${0%/*}/.."

# Have dependencies from npm ready.
npm i

# Compile application.
elm make src/Main.elm --output bin/elm-serve.js --optimize
elm-ffi bin/elm-serve.js --run --shebang
mv bin/elm-serve.js bin/elm-serve
