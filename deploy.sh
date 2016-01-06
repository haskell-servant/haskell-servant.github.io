REPO_URL="https://github.com/haskell-servant/haskell-servant.github.io"
SERVANT_WWW="$HOME/.servant-www"
SITE="$PWD/_site"
CURRDIR="$PWD"
BIN="dist/build/site/site"
COMMIT=$(git rev-parse HEAD)
BRANCH=$(git rev-parse --abbrev-ref HEAD)
MSG="Built from $COMMIT"

SERVANT_VERSIONS=(versions/0.2 versions/0.4 versions/0.5)
MAIN_BRANCH="main"

# Sanity check: documentation should only be built for one of the known major
# versions of servant.
check_tutorial_branch_is_correct () {
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

# Create directory to use for build
prepare_deploy () {
    if [ ! -d $SERVANT_WWW ]; then
        git clone $REPO_URL $HOME/.servant-www
        echo "Created directory $SERVANT_WWW"
    fi
    git rm -r $SERVANT_WWW/*
}

# Deploy tutorial
deploy_tutorial_version () {
    check_tutorial_branch_is_correct
    prepare_deploy
    cp -R $SERVANT_WWW/$SITE/tutorial/* $SERVANT_WWW/tutorial/$BRANCH
    git add $SERVANT_WWW/tutorial/$BRANCH
    git commit -m "Built tutorial for version $BRANCH from $COMMIT"
    git push origin master
}

deploy_main_version () {
    # TODO
}


if [[ "${BRANCH}" = "${MAIN_BRANCH}" ]] ; then
    deploy_main_version
else
    deploy_tutorial_version
fi
