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

unsetopt MULTIOS
setopt MAGIC_EQUAL_SUBST
setopt BSD_ECHO
setopt INTERACTIVE_COMMENTS
setopt PROMPT_SUBST

__git_files () {
    _wanted files expl 'local files' _files
}

[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

function docker-compose-watch() {
  local args;
  local watch_dir;

  if [[ $1 == "help" ]] || [[ $1 = "--help" ]]; then
    watchexec --help | grep -A 3 "OPTIONS:";
    return;
  else
    if [[ $1 == "-w" ]]; then
      watch_dir=$(readlink -f $2);
    else
      watch_dir=$(pwd);
    fi

    args='--filter "*/docker-compose.yml"' && [[ $1 ]] && args=$@;
  fi

  echo "Watching for changes in " $watch_dir;

  eval watchexec --restart $args -w $watch_dir "docker-compose up"
}

alias docker-compose-reload=docker-compose-watch;
