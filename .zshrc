# Environment variables
export ZSH=/home/wrongway4you/.oh-my-zsh
export UPDATE_ZSH_DAYS=16
export LANG=en_US.UTF-8
export EDITOR=vim

# ZSh options
ZSH_THEME="af-magic"
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
