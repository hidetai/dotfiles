#!/bin/sh
################################################################################
# install.sh
#
#   This script creates symbolic links from the home directory to dotfiles.
################################################################################

dotdir=$HOME/dotfiles

## zsh
ln -ns $dotdir/.zshrc $HOME

## screen
ln -ns $dotdir/.screenrc $HOME

## emacs
ln -ns $dotdir/.emacs.d/init.el $HOME/.emacs.d/

## vim
(cd $dotdir; git submodule init; git submodule update)
mkdir -p $HOME/.vim/bundle
ln -ns $dotdir/neobundle.vim $HOME/.vim/bundle/neobundle.vim
ln -ns $dotdir/.vimrc .vimrc
