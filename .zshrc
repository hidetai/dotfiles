# users generic .zshrc file for zsh(1)

#------------------------------------------------------------------------------
# Alias
#------------------------------------------------------------------------------

## ls
export LSCOLORS=exgxfxdxcxdxdxbxadacec
case "${OSTYPE}" in
darwin*)
    alias ls="ls -GF"
    ;;
linux*)
    alias ls="ls -F --color=auto"
    ;;
esac

alias la='ls -a'
alias ll='ls -al'

## Global Alias
alias -g L='| less'
alias -g G='| grep'

#------------------------------------------------------------------------------
# General
#------------------------------------------------------------------------------

## PATH
export PATH=/opt/local/bin:/opt/local/sbin:$PATH

## MANPATH
export MATHPATH=/opt/local/man:$MANPATH

## Complement
autoload -U compinit && compinit
setopt no_auto_menu
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

## Enable history search with Ctrl-P and Ctrl-N
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

## Save directory history
setopt auto_pushd

## Emacs key bind
bindkey -e

## Enable comments in command line
setopt interactive_comments

## Automatically run ls after every cd
function chpwd() {
    emulate -L zsh
    ls
}

## Disable console lock
stty stop undef

#------------------------------------------------------------------------------
# Look And Feel
#------------------------------------------------------------------------------

## Prompt
autoload colors
colors
PROMPT="%{${fg[magenta]}%}[%~]%{${reset_color}%}
%{${fg[magenta]}%}%m %(!.#.$) %{${reset_color}%}"
PROMPT2="%{${fg[magenta]}%}%_> %{${reset_color}%}"
SPROMPT="%{${fg[red]}%}correct: %R -> %r [nyae]? %{${reset_color}%}"
RPROMPT="%{${fg[magenta]}%}(%n)%{${reset_color}%}"

setopt transient_rprompt

## GNU Screen Titles
case "$TERM" in
screen)
    precmd() {
        echo -ne "\ek$(basename $(pwd))\e\\"
    }
esac

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

#------------------------------------------------------------------------------
# Load local file
#------------------------------------------------------------------------------

if [ -f $HOME/.zshrc.local ]; then
    source $HOME/.zshrc.local
fi
