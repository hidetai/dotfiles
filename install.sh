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
