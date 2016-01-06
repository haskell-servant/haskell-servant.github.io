REPO_URL="https://github.com/haskell-servant/haskell-servant.github.io"
SERVANT_WWW="$HOME/.servant-www"
SITE="$PWD/_site"
CURRDIR="$PWD"
BIN="dist/build/site/site"
COMMIT=$(git rev-parse HEAD)
BRANCH=$(git rev-parse --abbrev-ref HEAD)
MSG="Built from $COMMIT"

SERVANT_VERSIONS=(0.2 0.4 0.5)

# Sanity check: documentation should only be built for one of the known major
# versions of servant.
check_branch_is_correct () {
    if [[ " ${SERVANT_VERSIONS[@]} " =~ " ${BRANCH} " ]]; then
        # Check that version of servant used in cabal file corresponds to the
        # branch we are on.
        # TODO
    else
        echo "Branch is not a known version of servant!"
        echo "Known versions: ${SERVANT_VERSIONS[@]}"
        exit 1
    fi
}

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
