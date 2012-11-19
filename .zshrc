# users generic .zshrc file for zsh(1)

#------------------------------------------------------------------------------
# Alias
#------------------------------------------------------------------------------

alias ls="ls -G"
alias la='ls -a'
alias ll='ls -l'

#------------------------------------------------------------------------------
# General
#------------------------------------------------------------------------------

## PATH
export PATH=/opt/local/bin:/opt/local/sbin:$PATH

## MANPATH
export MATHPATH=/opt/local/man:$MANPATH

## Complement
autoload -U compinit && compinit
zstyle ':completion:*:default' menu select=1
zstyle ':completion:*' list-colors ''

## Enable comments in command line
setopt interactive_comments

## Save directory history
setopt auto_pushd

## Emacs key bind
bindkey -e

#------------------------------------------------------------------------------
# Look And Feel
#------------------------------------------------------------------------------

## Prompt
autoload colors
colors
PROMPT="%{${fg[magenta]}%}[%m:%~] %(!.#.$) %{${reset_color}%}"
PROMPT2="%{${fg[magenta]}%}%_> %{${reset_color}%}"
SPROMPT="%{${fg[red]}%}correct: %R -> %r [nyae]? %{${reset_color}%}"
RPROMPT="%{${fg[cyan]}%}[%n]%{${reset_color}%}"

export LSCOLORS=exfxcxdxbxegedabagacad

#------------------------------------------------------------------------------
# history
#------------------------------------------------------------------------------

HISTFILE="$HOME/.zsh_history"
HISTSIZE=10000000
SAVEHIST=10000000
## Save timestamp
setopt extended_history
## Append commands to history incrementaly
setopt inc_append_history
## Share history
setopt share_history
