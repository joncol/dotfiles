git checkout staging && git pull --ff-only
git checkout master && git pull --ff-only
git merge staging
# fix conflicts if needed
git push
git checkout staging
git merge master
git push
