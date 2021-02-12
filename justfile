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
