# Some functions
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

# Environment variables
export ZSH=~/.oh-my-zsh
export UPDATE_ZSH_DAYS=16
export LANG=en_US.UTF-8
export EDITOR=vim

source ~/.profile

# ZSh options
ZSH_THEME="awesomepanda"
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

alias rr="clear && printf '\e[3J'"

alias cclip='xclip -selection clipboard'
alias clipp='xclip -selection clipboard -o'

alias vi='vim'

alias gits='git status'
alias gitc='git commit'

unsetopt MULTIOS
setopt MAGIC_EQUAL_SUBST
setopt BSD_ECHO
setopt INTERACTIVE_COMMENTS
setopt PROMPT_SUBST

__git_files () {
    _wanted files expl 'local files' _files
}

[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

# Keybindings
bindkey "^[h" backward-delete-char
bindkey "^[d" delete-char

bindkey "^H" backward-kill-word
bindkey "^D" kill-word

bindkey "^[J" shift-left
bindkey "^[L" shift-right
bindkey "^[I" shift-up
bindkey "^[K" shift-down

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
