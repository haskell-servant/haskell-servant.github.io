XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
SERVANT_WWW="$XDG_CACHE_HOME/haskell-servant-github-io"

REPO_URL="git@github.com:haskell-servant/haskell-servant.github.io"
SITE="$PWD/_site"
COMMIT="$(git rev-parse HEAD)"

set -o errexit

# Build locally
cabal new-run -w ghc-8.4.3 site build

# Clone dir
if [ ! -d "$SERVANT_WWW" ]; then
	git clone "$REPO_URL" "$SERVANT_WWW"
	echo "Created directory $SERVANT_WWW"
fi


cd "$SERVANT_WWW"
git checkout master
git rm -r ./*
cp -R "$SITE"/* ./
git add ./**
git commit -m "Built from $COMMIT"
git push origin master
echo "Build from $COMMIT"
