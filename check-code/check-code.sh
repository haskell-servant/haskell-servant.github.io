#!/usr/bin/env bash

set -o errexit

cabal exec -- ghc -outputdir build-output ../tutorial/api-type.lhs -O0 -c
cabal exec -- ghc -outputdir build-output ../tutorial/server.lhs -O0 -c -fno-warn-missing-methods
cabal exec -- ghc -outputdir build-output ../tutorial/client.lhs -O0 -c -fno-warn-missing-methods
