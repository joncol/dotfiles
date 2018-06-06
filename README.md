# Jonas' dotfiles

Configuration for Linux based system, with fish, mail settings etc.

## Requirements

GNU Stow (https://www.gnu.org/software/stow/manual/stow.html).

## Usage

To install symbolic links to all configuration files (assuming you've cloned
this repo to `~/.dotfiles`):

```bash
stow *
```
To uninstall all symbolic links:

```bash
stow -D *
```

Note that the custom `i3lock-color` needs to be available on the PATH for the
locking to work:

```bash
sudo ln -sf ~/.dotfiles/bin/.local/bin/i3lock /usr/local/bin/i3lock
```

## License

Copyright © 2018 Jonas Collberg
