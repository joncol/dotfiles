# Defined in /tmp/fish.8qjT0Z/gitstatus.fish @ line 2
function gitstatus
	find . -maxdepth 1 -mindepth 1 -type d -exec sh -c '(if [ -d "{}/.git" ]; then echo {} && cd {} && git status -s && echo; fi)' \;
end
