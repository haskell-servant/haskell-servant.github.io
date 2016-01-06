#!/usr/bin/env bash

set -o errexit

cabal exec -- ghc -outputdir build-output ../tutorial/api-type.lhs
