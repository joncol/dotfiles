#! /usr/bin/env sh

set -o errexit  # Abort on nonzero exitstatus.
set -o nounset  # Abort on unbound variable.
set -o pipefail # Don't hide errors within pipes.

day=$(date +%Y-%m-%d)
git checkout production
git pull --ff-only
git tag "production-$day"
git push -u origin "production-$day:production-$day"
git reset --hard staging
git push --force-with-lease
