REPO_URL="git@github.com:algas/haskell-servant.github.io"
SERVANT_WWW="$HOME/.servant-www"
SITE="$PWD/_site"
CURRDIR="$PWD"
BIN="dist/build/site/site"
COMMIT=`git rev-parse HEAD`
MSG="Built from $COMMIT"

set -o errexit

if [ ! -d $SERVANT_WWW ]; then
	git clone $REPO_URL $HOME/.servant-www
	echo "Created directory $SERVANT_WWW"
fi

cd $SERVANT_WWW
git checkout gh-pages
git rm -r ./*
cp -R $SITE/* ./ 
git add ./**
git commit -m "Built from $COMMIT"
git push origin gh-pages
echo $MSG
cd $CURRDIR
