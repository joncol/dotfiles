#!/usr/bin/env bash

set -o errexit  # Abort on nonzero exitstatus.
set -o nounset  # Abort on unbound variable.
set -o pipefail # Don't hide errors within pipes.
set -o xtrace # Trace all commands being executed.

date=$(date +%Y-%m-%d)
git checkout production
git pull --ff-only origin
git tag "production-${date}" # Create a tag.
git push -u origin production-${date}:production-${date} # Push the tag.
git reset --hard staging # Production branch is already merged into staging, so we can reset our local production branch to that.
git tag release/${RELEASE_NUMBER}
git push --force-with-lease origin # Push merge of staging and old production into production branch.
git push origin release/${RELEASE_NUMBER} # Push the tag
