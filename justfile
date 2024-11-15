# Local Variables:
# mode: makefile
# indent-tabs-mode: nil
# End:
# vim: set ft=make expandtab shiftwidth=2:

# Install .desktop files for XMonad. Note that LightDM doesn't find XMonad if we
# use a symbolic link for `/usr/share/xsessions/xmonad.desktop`, so that file
# needs to be created in place.
xmonad:
  #!/usr/bin/env bash
  sudo stow -t / xmonad
  cat <<EOF | sudo tee /usr/share/xsessions/xmonad.desktop > /dev/null
  [Desktop Entry]
  Encoding=UTF-8
  Name=XMonad
  Comment=Lightweight tiling window manager
  Exec=xmonad
  Icon=xmonad.png
  Type=XSession
  EOF

# Create Git hooks to make sure that the version of `init.el` that is committed
# corresponds to the correct version of `init.org`, and to automatically prepare
# the commit message.
git-hooks:
  #!/usr/bin/env bash
  cat <<"EOF" > .git/hooks/pre-commit
  #!/bin/sh

  init_org_file=$(
    git diff --cached --find-copies --find-renames --name-only --diff-filter=ACMRTXBU |
    grep -E "init\.org"
  )

  if [[ -n $init_org_file ]];
  then
    # Create a temp file
    TMPFILE=`mktemp` || exit 1

    # Set mode in temp file.
    echo "-*- mode: org -*-" >> $TMPFILE

    # Write the staged version of `init.org`.
    git show :homedir/.emacs.d/init.org >> $TMPFILE

    # Tangle the temp file.
    TANGLED=`emacsclient -e "(let ((enable-local-variables :safe)) (car (org-babel-tangle-file \"$TMPFILE\")))"`

    # Overwrite .emacs.d/init.el with the file that is based on the staged changes.
    mv -f "${TANGLED//\"}" homedir/.emacs.d/init.el

    # Stage the file
    git add homedir/.emacs.d/init.el
  fi
  EOF

  chmod +x .git/hooks/pre-commit

  cat <<"EOF" > .git/hooks/post-commit
  #!/bin/sh

  # Retangle `init.org` as it is, so all changes are reflected in `init.el`.
  emacsclient -e "(let ((enable-local-variables :safe)) (car (org-babel-tangle-config)))"
  EOF

  chmod +x .git/hooks/post-commit

  cat <<"EOF" > .git/hooks/prepare-commit-msg
  #!/bin/sh

  COMMIT_MSG_FILE=$1
  COMMIT_SOURCE=$2
  SHA1=$3

  # Automatically prefix the commit message for certain files.
  # First delete (`!d`) any lines not matching the pattern.
  # Then, replace some longer path with a shorter prefix, such as `nvim:` for
  # any file in the `~/.config/nvim` directory.
  case "$COMMIT_SOURCE,$SHA1" in
   ,|template,)
     /usr/bin/env perl -i.bak -pe '
       if ($first++ == 0) {
         print `git diff --cached --name-only | sed -E "/\(\\.org\\b|\\.config\\/nvim\)/!d" | \
           sed -z "s;^.*/.emacs.d/init.org;emacs: ;" | \
           sed -z "s;^.*/.config/nvim/.*;nvim\: ;"`
       }' "$COMMIT_MSG_FILE";;
   *) ;;
  esac
  EOF
  chmod +x .git/hooks/prepare-commit-msg

# Make git ignore certain files where only local changes are applied.
skip_worktree:
  git update-index --skip-worktree homedir/.emacs.d/custom.el

# Log in to the Docker registry of Scrive.
docker-login:
  aws ecr get-login-password --profile scrive | docker login --username AWS --password-stdin 720173602891.dkr.ecr.eu-west-1.amazonaws.com
