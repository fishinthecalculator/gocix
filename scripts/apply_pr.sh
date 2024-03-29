#!/usr/bin/env bash

set -eu

main="main"

usage () {
    echo "Usage: $(basename "$0") FORK-URL BRANCH" >&2
}


[ "$#" -ne 2 ] && usage && exit 1

fork_url="$1"
branch="$2"

git checkout "${main}"

# Setup
git remote add forked "$fork_url"
git fetch forked "${branch}"

git cherry-pick -s "${main}..forked/${branch}"

# Cleanup
git branch -r -D "forked/${branch}"
git remote remove forked

# Run guix git authenticate
guix shell make -- make authenticate
