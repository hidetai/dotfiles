#!/bin/sh
################################################################################
# install.sh
#
#   This script creates symbolic links from the home directory to dotfiles.
################################################################################

dotdir=$HOME/dotfiles

# git submodule
(cd $dotdir; git submodule init; git submodule update)

## zsh
ln -ns $dotdir/.zshrc $HOME

## screen
ln -ns $dotdir/.screenrc $HOME

## emacs
if [ ! -d $HOME/.emacs.d/ ]; then
    mkdir $HOME/.emacs.d/
fi
ln -ns $dotdir/.emacs.d/init.el $HOME/.emacs.d/

## vim
mkdir -p $HOME/.vim/bundle
ln -ns $dotdir/neobundle.vim $HOME/.vim/bundle/neobundle.vim
ln -ns $dotdir/.vimrc $HOME/.vimrc

## git
ln -ns $dotdir/.gitconfig $HOME/.gitconfig
