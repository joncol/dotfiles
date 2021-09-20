# Local Variables:
# mode: makefile
# indent-tabs-mode: nil
# End:
# vim: set ft=make :

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
git_hooks:
  #!/usr/bin/env bash
  cat <<"EOF" > .git/hooks/pre-commit
  #!/bin/sh

  # Create a temp file
  TMPFILE=`mktemp` || exit 1

  # Set mode in temp file.
  echo "-*- mode: org -*-" >> $TMPFILE

  # Write the staged version of `init.org`.
  git show :homedir/.config/emacs/init.org >> $TMPFILE

  # Tangle the temp file.
  TANGLED=`emacsclient -e "(let ((enable-local-variables :safe)) (car (org-babel-tangle-file \"$TMPFILE\")))"`

  # Overwrite .emacs.d/init.el with the file that is based on the staged changes.
  mv -f "${TANGLED//\"}" homedir/.config/emacs/init.el

  # Stage the file
  git add homedir/.config/emacs/init.el
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

  # Print the correct prefix in the commit message.
  case "$COMMIT_SOURCE,$SHA1" in
   ,|template,)
     /usr/bin/perl -i.bak -pe '
        print `git diff --cached --name-only | sed "/\\.org\\b/!d" | \
            sed "s:^.*/emacs/init.org:emacs:" | \
            sed -z "s/\\n/: \\n/g"`
       if $first++ == 0' "$COMMIT_MSG_FILE" ;;
   *) ;;
  esac
  EOF
  chmod +x .git/hooks/prepare-commit-msg

# Make git ignore certain files where only local changes are applied.
skip_worktree:
  git update-index --skip-worktree homedir/.config/emacs/custom.el
