# Jonas' dotfiles

My personal configuration for Linux based system, with `zsh`, email settings
etc.

## Requirements

GNU Stow (https://www.gnu.org/software/stow/manual/stow.html).

## Usage

To install symbolic links to all home directory configuration files (assuming
you've cloned this repo to `~/code/dotfiles`):

```bash
cd ~/code/dotfiles/homedir
stow -t ~ *
```

Or, standing directly in the repository root:

```bash
stow -t ~ homedir
```

To uninstall all symbolic links to home directory configuration files:

```bash
cd ~/code/dotfiles/homedir
stow -Dt ~ homedir
```

To install work setup files:
```bash
stow -t ~/work work
```

To uninstall work setup files:

```bash
stow -Dt ~/work work
```

## License

Copyright © 2018 Jonas Collberg
