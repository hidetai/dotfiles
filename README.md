Dotfiles
======================

My config files for vim, zsh, and screen.

Installation
------------

Clone from github.

    cd ~
    git clone https://github.com/hidetai/dotfiles.git dotfiles

Create symbolic links.

    cd ~
    ./dotfiles/install.sh

Vim Setup
---------

Launch vim, and execute `:NeoBundleInstall` to install bundles.

    :NeoBundleInstall

Zsh Setup
---------

Show the path of zsh.

    which zsh

Change a login shell to zsh.

    chsh

Updating
--------

    cd ~/dotfiles
    git pull
