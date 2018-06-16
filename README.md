# Jonas' dotfiles

Configuration for Linux based system, with fish, mail settings etc.

## Requirements

GNU Stow (https://www.gnu.org/software/stow/manual/stow.html).

## Usage

To install symbolic links to all home directory configuration files (assuming
you've cloned this repo to `~/code/dotfiles`):

```bash
cd ~/code/dotfiles/homedir
stow -t ~ *
```
To uninstall all symbolic links to home directory configuration files:

```bash
cd ~/code/dotfiles/homedir
stow -t ~ -D *
```

Note that `i3lock-color` needs to be installed. It replaces `i3lock`.

## License

Copyright © 2018 Jonas Collberg
