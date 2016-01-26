# Rob's dotfiles

My configuration files for various applications. I clone this repository on to each machine, then symlink each file in the relevant location (e.g., `ln -s ~/dotfiles/emacs ~/.emacs`). Any machine-specific configuration should be done in a branch. Obviously, all configuration reflects my personal preferences.

## .emacs

The `.emacs` file is self-contained; complex packages are assumed to be handled separately by the packaging system. Notable features:

* The general look is light-on-dark. I don't use a theme.
* Text mode (and derivatives such as markdown-mode) use variable pitch and visual line modes to provide a more word processor like environment for editing text. They also enable flyspell by default, with a custom word identification function that skips things that lookLikeCode.
* The server is started on launch.
* Meta is the command key, plus Mac-like bindings for M-c, M-v, M-z and M-s. You can take my M-x when you pry it from my cold, dead hands.

## .bashrc

The `.bashrc` isn't all that complicated, but contains a few useful snippets:

* A function `source_if_present` to source a file if it's present (and do nothing otherwise); this is used to support `.bashrc.local` for machine specific configuration.
* A function `pathadd` to add a directory to the front of `$PATH`, *removing any duplicates that are already there* to prevent pernicious path growth.
* A useful but not overly complicated prompt including:
  * The working directory
  * The exit status of the last command, if non-zero
  * The current Python virtualenv, if any
  * The current git branch, if any, plus a red dot if the working directory has uncommitted changes

I've not put too much effort into portability, but it works for me on both OS X and Linux.

## org.jupyter.notebook.plist

A simple `launchd` configuration to start a [Jupyter](http://jupyter.org) notebook server at login. To use, copy or symlink it to `~/Library/LaunchAgents`, and modify the directory to match your local setup.
