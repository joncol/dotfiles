#!/usr/bin/env bash

declare -A git_projects
dirlist=()

while IFS= read -d $'\0' -r file; do
    dir=$(basename "$(dirname "$file")")
    git_projects["$dir"]="$(dirname "$file")"
    dirlist+="$dir\n"
done < <(fd -H -t d --print0 '^.git$' ~)

choice=$(echo -e "$dirlist" | dmenu -i -p "Select project")
if [ -n "$choice" ]; then
    urxvt -cd "${git_projects["$choice"]}"
fi
