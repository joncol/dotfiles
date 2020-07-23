# Defined in /tmp/fish.F8xGKJ/git-diff-sops.fish @ line 2
function git-diff-sops
    diff -u (sops -d (git --no-pager show master:$argv[1] | psub) | psub) (sops -d $argv[1] | psub) | tee /dev/tty | xclip
end
