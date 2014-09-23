# Rob's dotfiles

My configuration files for various applications. I clone this repository on to each machine, then symlink each file in the relevant location (e.g., `ln -s ~/dotfiles/emacs ~/.emacs`). Any machine-specific configuration should be done in a branch. Obviously, all configuration reflects my personal preferences.

## .emacs

The `.emacs` file is self-contained; complex packages are assumed to be handled separately by the packaging system. Notable features:

* The general look is light-on-dark. I don't use a theme.
* Text mode (and derivatives such as markdown-mode) use variable pitch and visual line modes to provide a more word processor like environment for editing text. They also enable flyspell by default, with a custom word identification function that skips things that lookLikeCode.
* The server is started on launch.
