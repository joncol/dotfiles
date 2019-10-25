# Defined in /tmp/fish.crBfHH/pip-update-all-packages.fish @ line 1
function pip-update-all-packages
	pip list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f1 | xargs -n1 pip install --user -U
end
