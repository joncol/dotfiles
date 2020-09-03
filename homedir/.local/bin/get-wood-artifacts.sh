#!/usr/bin/env bash
for REMOTE in $(aws s3 ls s3://ebway/artifacts/production/$(date +"%Y-%m-%d") | grep "$1" | rev | cut -d " " -f 1 | rev)
do
  aws s3 cp "s3://ebway/artifacts/production/$REMOTE" "artifact/production/$REMOTE"
done
