#!/usr/bin/env bash

set -o errexit

tinc

cabal exec -- ghc -Wall -Werror -outputdir build-output ../tutorial/api-type.lhs -O0 -c
cabal exec -- ghc -Wall -Werror -outputdir build-output ../tutorial/server.lhs -O0 -c -fno-warn-missing-methods -fno-warn-name-shadowing
cabal exec -- ghc -Wall -Werror -outputdir build-output ../tutorial/client.lhs -O0 -c -fno-warn-missing-methods -fno-warn-name-shadowing
cabal exec -- ghc -Wall -Werror -outputdir build-output ../tutorial/javascript.lhs -O0 -c -fno-warn-missing-methods
cabal exec -- ghc -Wall -Werror -ibuild-output -outputdir build-output ../tutorial/docs.lhs -O0 -c -fno-warn-missing-methods
cabal exec -- runghc -Wall -Werror -fno-warn-missing-methods ../posts/2016-02-06-servant-swagger.lhs -O0 -pgmL markdown-unlit -c
