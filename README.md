Instructions:

``` bash
$ git clone https://github.com/haskell-servant/haskell-servant.github.io.git
$ cd haskell-servant.github.io/
$ git checkout hakyll
$ cabal sandbox init && cabal install --dep
# go for a walk
$ cabal build
$ dist/build/site/site watch
# builds the site and serves it at localhost:8000
# any changes to the content immediately regenerates the relevant parts of the website
$ dist/build/site/site build && dist/build/site/site deploy
# builds the site and commits/pushes the result to http://haskell-servant.github.io -- the 'master' branch of this repo
```
