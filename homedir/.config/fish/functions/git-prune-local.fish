# Defined in /tmp/fish.54aeAk/git-prune-local.fish @ line 1
function git-prune-local
	git branch -r | awk '{print $1}' | grep -vf /dev/fd/0 (git branch -vv | grep origin | psub) | awk '{print $1}' | xargs git branch -D
end
