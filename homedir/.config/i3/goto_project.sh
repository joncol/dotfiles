#!/usr/bin/env bash

declare -A git_projects
dirlist=()

while IFS= read -d $'\0' -r file; do
    dir=$(basename "$(dirname "$file")")

    hidden=0
    IFS='/' read -ra dir_parts <<< "$file"
    unset 'dir_parts[${#dir_parts[@]}-1]'
    for p in "${dir_parts[@]}"; do
        [[ "$p" =~ (^\.|___) ]] && hidden=1 && break
    done
    if [ "$hidden" -ne 0 ]; then continue; fi

    git_projects["$dir"]="$(dirname "$file")"
    dirlist+="$dir\n"
done < <(fd -H -t d -0 '^.git$' ~)

choice=$(echo -e "$dirlist" | dmenu -i -p "Select project")
if [ -n "$choice" ]; then
    urxvt -cd "${git_projects["$choice"]}"
fi
