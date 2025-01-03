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
git fetch origin keyring:keyring
guix git authenticate --cache-key=channels/gocix cdb78996334c4f63304ecce224e95bb96bfd4c7d '8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2'
