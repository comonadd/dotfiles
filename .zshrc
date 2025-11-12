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
    alias ls="eza --icons"
    alias la="eza --icons -la"
    alias cat="bat"
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
    alias chrome_insecure='open -n -a /Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome --args --user-data-dir=/tmp/chrome_insecure --disable-web-security'
    alias kill_workers="ps aux | grep split_translate_router | awk '{print $2}' | xargs kill -9"
}

# Neovim with tab-specific server (uses iTerm session ID for unique socket)
# Replace colons in session ID as they're invalid in socket paths
unalias vim 2>/dev/null || true
unalias nvim 2>/dev/null || true

nvim() {
    local socket_name="${ITERM_SESSION_ID:-default}"
    socket_name="${socket_name//:/—}"
    command nvim --listen "/tmp/nvimsocket-$socket_name" "$@"
}

vim() {
    nvim "$@"
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

function make() {
    if [[ -f Makefile.local ]]; then
        echo "Using Makefile.local"
        command make -f Makefile.local "$@"
    elif [[ -f Makefile.new ]]; then
        echo "Using Makefile.new"
        command make -f Makefile.new "$@"
    else
        command make "$@"
    fi
}

# source ~/zsh-autocomplete/zsh-autocomplete.plugin.zsh

setup_aliases
setup_docker_commands


####################
### ZSH settings ###
####################

# Silence direnv output completely (set before loading plugins)
export DIRENV_LOG_FORMAT=""
export DIRENV_WARN_TIMEOUT="0"

# Custom direnv hook to silence all output
_direnv_hook() {
  eval "$(direnv export zsh 2>/dev/null)" 2>/dev/null;
}

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
    fzf
    direnv
    pyenv
    docker
    kubectl
    colored-man-pages
    command-not-found
    macos
    npm
    yarn
    zsh-autosuggestions
    zsh-syntax-highlighting
)

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

eval "$(zoxide init zsh)"

###################
### Completions ###
###################

[[ $commands[kubectl] ]] && source <(kubectl completion zsh)

# uv completion
if command -v uv &> /dev/null; then
    eval "$(uv generate-shell-completion zsh)"
fi

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
alias cdb="cd ~/Work/visible-web/backend"
alias cda="cd ~/Work/visible-web/frontend/admin"
alias cdnvim="cd ~/.config/nvim"

alias claude='unset -f _direnv_hook 2>/dev/null; command claude'

alias ag='ag --hidden --ignore .git'

export PATH="$PATH:$HOME/.cargo/bin"


alias list-git-skip-worktree-files="git ls-files -v | grep '^S'"

export HOMEBREW_NO_ENV_HINTS=1
