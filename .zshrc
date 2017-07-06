# Environment variables
export ZSH=~/.oh-my-zsh
export UPDATE_ZSH_DAYS=16
export LANG=en_US.UTF-8
export EDITOR=vim

source ~/.profile

# ZSh options
ZSH_THEME="agnoster"
CASE_SENSITIVE="false"
HYPHEN_INSENSITIVE="true"
DISABLE_AUTO_UPDATE="true"
DISABLE_LS_COLORS="false"
DISABLE_AUTO_TITLE="true"
ENABLE_CORRECTION="false"
COMPLETION_WAITING_DOTS="false"
DISABLE_UNTRACKED_FILES_DIRTY="true"
HIST_STAMPS="mm/dd/yyyy"
plugins=(git)

source $ZSH/oh-my-zsh.sh
stty -ixon

# Aliases
alias cdA="cd ~/.config/awesome"
alias cdV="cd ~/.vim"

alias eX="$EDITOR ~/.Xresources"
alias eA="$EDITOR ~/.config/awesome/rc.lua"
alias eV="$EDITOR ~/.vimrc"
alias eZ="$EDITOR ~/.zshrc"

alias uX="xrdb ~/.Xresources"

alias make="make --warn-undefined-variables"

unsetopt MULTIOS
setopt MAGIC_EQUAL_SUBST
setopt BSD_ECHO
setopt INTERACTIVE_COMMENTS
setopt PROMPT_SUBST

__git_files () {
    _wanted files expl 'local files' _files
}

alias rr="clear && printf '\e[3J'"
