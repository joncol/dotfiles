#!/usr/bin/env bash

set -o errexit  # Abort on nonzero exitstatus.
set -o nounset  # Abort on unbound variable.
set -o pipefail # Don't hide errors within pipes.

date=$(date +%Y-%m-%d)
release_number=${1:-} # Don't forget to update this!

if [ -n "${release_number}" ]; then
  ./scripts/generate-release-notes-md.sh "production-${date}" production "Release ${release_number}" \
    | tee "doc/release_notes/Release_Notes_${release_number}.md"
else
  echo "Please add a release number as a CLI option."
fi
