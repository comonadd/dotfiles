# Movement functions
shift-arrow() {
    ((REGION_ACTIVE)) || zle set-mark-command
    zle $1
}
shift-left() shift-arrow backward-char
shift-right() shift-arrow forward-char
shift-up() shift-arrow up-line-or-history
shift-down() shift-arrow down-line-or-history
zle -N shift-left
zle -N shift-right
zle -N shift-up
zle -N shift-down

# Pull all remote Git branches
# NOTE: This function is based on [this](https://stackoverflow.com/questions/10312521/how-to-fetch-all-git-branches)
#       StackOverflow answer.
pull-all-git-branches() {
    git branch -r | grep -v '\->' | while read remote; do git branch --track "${remote#origin/}" "$remote"; done
    git fetch --all
    git pull --all
    for remote in `git branch -r`; do git branch --track ${remote#origin/} $remote; done
}

# Environment variables
export ZSH=~/.oh-my-zsh
export UPDATE_ZSH_DAYS=16
export LANG=en_US.UTF-8
export EDITOR=vim

source ~/.profile

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

# CD Aliases
alias cdA="cd ~/.config/awesome"
alias cdV="cd ~/.vim"

# EDIT aliases
alias eX="$EDITOR ~/.Xresources"
alias eA="$EDITOR ~/.config/awesome/rc.lua"
alias eV="$EDITOR ~/.vimrc"
alias eZ="$EDITOR ~/.zshrc"

# Clipboard management aliases
alias cclip='xclip -selection clipboard'
alias clipp='xclip -selection clipboard -o'

# Command flag modification aliases
alias make="make --warn-undefined-variables"

# Git aliases
alias gits='git status'
alias gitc='git commit'
alias gitpab=pull-all-git-branches

# Other aliases
alias uX="xrdb ~/.Xresources"
alias rr="clear && printf '\e[3J'"
alias vi='vim'
alias neovim='nvim'
alias pip='pip3'

unsetopt MULTIOS
setopt MAGIC_EQUAL_SUBST
setopt BSD_ECHO
setopt INTERACTIVE_COMMENTS
setopt PROMPT_SUBST

__git_files () {
    _wanted files expl 'local files' _files
}

[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

# Enable Vim mode
bindkey -v

# Initialize NVM
# source /usr/share/nvm/init-nvm.sh
