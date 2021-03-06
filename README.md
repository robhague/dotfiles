# Rob's dotfiles

My configuration files for various applications. I clone this repository on to each machine, then symlink each file in the relevant location (e.g., `ln -s ~/dotfiles/emacs ~/.emacs`). Any machine-specific configuration should be done in a branch. Obviously, all configuration reflects my personal preferences.

## .emacs

The `.emacs` file is self-contained; complex packages are assumed to be handled separately by the packaging system. Notable features:

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

## gitconfig

Some general-purpose Git aliases and preferences. The aliases are:

* `update`: Pull, and then update submodules
* `tidymerged`: Checks out master, updates (as above), then delete any local branches that have been merged into master
* `diffw`: Simple shortcut for word difference
* `branchname`: Print the name of the current branch
* `pushup`: Push to origin, setting the upstream branch name to match the local one
* `browse`: Browse the current repo's web page, assuming that it has a more-or-less Github-like setup and there's a command called `open` that can handle URLs (as there is on macOS)

Yes, I use submoudles.

In addition, the file specifies a filer for Jupyter/IPython notebooks, as described in [this blog post](http://pascalbugnion.net/blog/ipython-notebooks-and-git.html). My version will by default erase execution counts but leave output intact, but this behaviour can be configured by modifying the notebook metadata. For example:

    # Erase both execution count and output
    "git": { "clean": { "execution_counts": true, "output": true } }

    # Leave the file unmodified
    "git": { "clean": false }

Note that the filter assumes you've installed this repository as `~/dotfiles`.

## gitattributes

Specifies that the above filter should be used for files matching `*.ipynb`. This can be linked to `.gitattributes` in your repository, or `.config/git/attributes` to apply globally.

## org.jupyter.notebook.plist

A simple `launchd` configuration to start a [Jupyter](http://jupyter.org) notebook server at login. To use, copy or symlink it to `~/Library/LaunchAgents`, and modify the directory to match your local setup.
