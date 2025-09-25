#!/bin/bash

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export ZSH=~/.oh-my-zsh
export UPDATE_ZSH_DAYS=16
export EDITOR=vim
export CC=/usr/bin/clang
export CXX=/usr/bin/clang++

IS_MACOS=$(uname -a | grep Darwin)

if [[ $IS_MACOS ]]; then
    # MACOS
    export CLICOLOR=YES
else
    # LINUX
    alias ls="ls --color=al"
    # Load proper LS_COLORS configuration
    [ -e ~/.dircolors ] && eval $(dircolors -b ~/.dircolors) ||
        eval $(dircolors -b)
fi

if [[ $IS_MACOS ]]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

#################
### Functions ###
#################

function setup_docker_commands() {
    alias docker-ls-all-containers="docker ps -aq"
    alias docker-stop-all-containers="docker stop (docker ps -aq)"
    alias docker-rm-all-containers="docker rm \$(docker ps -aq)"
    alias docker-rm-all-images="docker rmi \$(docker images -q)"
    function docker-run-mysql-dev-container () {
    echo "Starting MySQL development database Docker container";
    cd ~/Docker/local-mysql-db && sudo docker-compose up
    }
    function docker-run-rabbitmq-dev-container () {
    echo "Starting RabbitMQ development Docker container";
    cd ~/Docker/local-rabbitmq && sudo docker-compose up
    }
    function docker-run-postgres-dev-container () {
    echo "Starting PostgreSQL development database Docker container";
    cd ~/Docker/local-postgres-db && sudo docker-compose up
    }
    function docker-run-redis-dev-container () {
    echo "Starting Redis development database Docker container";
    cd ~/Docker/local-redis-db && sudo docker-compose up
    }
    function docker-run-mongo-dev-container () {
    echo "Starting MongoDB development Docker container";
    cd ~/Docker/local-mongo-db && sudo docker-compose up
    }
}

function setup_aliases() {
    alias python="python3"
    alias la="ls -la"
    alias vim="nvim"
    alias gits="git status"
    alias gitb="git branch | sed 's/^..//'"
    alias yt="youtube-dl --add-metadata -ic"
    alias yta="youtube-dl --add-metadata -xic --audio-format mp3 --audio-quality 0"
    alias killbg="kill $(jobs -p)"
    alias pip="pip3"
    alias evc="$EDITOR ~/.config/nvim/init.vim"
    alias tmux="tmux attach || tmux new"
    alias poetry-m1="LDFLAGS="-I/opt/homebrew/opt/openssl/include -L/opt/homebrew/opt/openssl/lib" poetry"
    alias gitcampf="git cam && git pf"
    alias act="act pull_request -P self-hosted=node:16-buster-slim"
    alias chrome_insecure='open -n -a /Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome --args --user-data-dir=/tmp/chrome_insecure --disable-web-security'
    alias kill_workers="ps aux | grep split_translate_router | awk '{print $2}' | xargs kill -9"
}

function load_dotenv() {
  echo "Loading environment from $1";
  export $(cat $1 | xargs)
}

kill_listeners() {
    if [ $# -ne 1 ]; then
        echo "Usage: kill_listeners <port>"
        return 1
    fi

    kill -9 $(lsof -t -i:$1)
    echo "Killed any processes listening on port $1"
}

function listening() {
    echo "Checking if any process is listening on port $1"
    if [ $# -eq 0 ]; then
        sudo lsof -iTCP -sTCP:LISTEN -n -P
    elif [ $# -eq 1 ]; then
        sudo lsof -iTCP -sTCP:LISTEN -n -P | grep -i --color $1
    else
        echo "Usage: listening [pattern]"
    fi
}

function no-quarantine {
    sudo xattr -d com.apple.quarantine $@
}

# source ~/zsh-autocomplete/zsh-autocomplete.plugin.zsh

setup_aliases
setup_docker_commands

eval "$(direnv hook zsh)"


####################
### ZSH settings ###
####################

ZSH_THEME="arrow"

PROMPT='%B%F{240}%1~%f%b %# '

force_color_prompt=yes

zstyle ':omz:update' mode auto      # update automatically without asking
zstyle ':omz:update' frequency 13

# Uncomment the following line to disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="false"

DISABLE_UNTRACKED_FILES_DIRTY="true"
HIST_STAMPS="dd.mm.yyyy"

plugins=(
    git
    gitfast
    git-extras
)

# Function to check and load environment when changing directory
function auto_env {
  # Check if .env file exists and load it using direnv
  # if [[ -f .env ]]; then
  #   eval "$(direnv dotenv)"
  # fi
  
  # Check if .venv or venv virtual environment exists and activate it
  if [[ -f .venv/bin/activate ]]; then
    source .venv/bin/activate
  elif [[ -f venv/bin/activate ]]; then
    source venv/bin/activate
  fi
}

# Hook the function to run every time you change directory
function chpwd {
  auto_env
}

source $ZSH/oh-my-zsh.sh

###################
### Keybindings ###
###################

bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
bindkey "^[[1;3C" forward-word
bindkey "^[[1;3D" backward-word


####################################
### Interpreter version managers ###
####################################

eval "$(fnm env --use-on-cd)"

export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init - zsh)"

###################
### Completions ###
###################

[[ $commands[kubectl] ]] && source <(kubectl completion zsh)

zstyle ':completion:*:*:make:*' tag-order 'targets'



# autoload -U compinit && compinit 

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

DATABASE_URL=postgres://postgres:postgres@db:5432/postgres


# autoload bashcompinit
# bashcompinit
# source ~/.bash_completion.d/python-argcomplete.sh
export PATH="/opt/homebrew/opt/libpq/bin:$PATH"

buffer-single-to-double-quotes() {
  pbpaste | sed "s/'/\"/g" | pbcopy
  echo "Replaced single quotes with double quotes in clipboard"
}

zstyle ':completion:*:*:git:*' user-commands checkout-local:'checkout showing only local branches'
zstyle ':completion:*:*:git-checkout-local:*' command 'git checkout -b'
zstyle ':completion:*:*:git-checkout-local:*:git-checkout-b' command 'git branch --no-color -l | sed "s/^..//"'


HISTDB_TABULATE_CMD=(sed -e $'s/\x1f/\t/g')
source $HOME/.oh-my-zsh/custom/plugins/zsh-histdb/sqlite-history.zsh
autoload -Uz add-zsh-hook

. "$HOME/.local/bin/env"

alias cdw="cd ~/Work/visible-web"
alias cdb="cd ~/Work/visible-web && pipenv shell && cd backend"
alias cda="cd ~/Work/visible-web/frontend/admin"
