REPO_URL="https://github.com/haskell-servant/haskell-servant.github.io"
SERVANT_WWW="$HOME/.servant-www"
SITE="$PWD/_site"
CURRDIR="$PWD"
BIN="dist/build/site/site"
COMMIT=`git rev-parse HEAD`
MSG="Built from $COMMIT"

if [ ! -d $SERVANT_WWW ]; then
	git clone $REPO_URL $HOME/.servant-www
	echo "Created directory $SERVANT_WWW"
fi

cd $SERVANT_WWW
git rm -r ./*
cp -R $SITE/* ./ 
git add ./**
git commit -m "Built from $COMMIT"
git push origin master
echo $MSG
cd $CURRDIR
