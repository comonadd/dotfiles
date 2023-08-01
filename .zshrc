# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="fino"

# Set list of themes to pick from when loading at random
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment one of the following lines to change the auto-update behavior
# zstyle ':omz:update' mode disabled  # disable automatic updates
# zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
# zstyle ':omz:update' frequency 13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

DISABLE_UNTRACKED_FILES_DIRTY="true"
HIST_STAMPS="dd.mm.yyyy"

plugins=(
    git
    gitfast
    zsh-syntax-highlighting
    zsh-autosuggestions
    zsh-history-substring-search
)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# export NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
# [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
# export PYENV_ROOT="$HOME/.pyenv"
# command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
# eval "$(pyenv init -)"
[[ $commands[kubectl] ]] && source <(kubectl completion zsh)
export PATH="/opt/homebrew/opt/ruby@3.1/bin:$PATH"

function load_dotenv() {
  echo "Loading environment from $1";
  export $(cat $1 | xargs)
}

export C_INCLUDE_PATH=$C_INCLUDE_PATH:/opt/homebrew/include
export LIBRARY_PATH=$LIBRARY_PATH:/opt/homebrew/lib

kill_listeners() {
    if [ $# -ne 1 ]; then
        echo "Usage: kill_listeners <port>"
        return 1
    fi

    kill -9 $(lsof -t -i:$1)
    echo "Killed any processes listening on port $1"
}

bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

function cd() {
  builtin cd "$@"
  VENV_DIR_NAMES=('.venv' 'venv')

  if [[ -z "$VIRTUAL_ENV" ]] ; then
    # If .venv or venv directory is found then activate the virtualenv
    for dir_name in "${VENV_DIR_NAMES[@]}"; do
      if [[ -d ./$dir_name ]] ; then
        source ./$dir_name/bin/activate
        break
      fi
    done
  else
    # check the current folder belong to earlier VIRTUAL_ENV folder
    # if yes then do nothing
    # else deactivate
    parentdir="$(dirname "$VIRTUAL_ENV")"
    if [[ "$PWD"/ != "$parentdir"/* ]] ; then
      deactivate
    fi
  fi
}

alias act="act pull_request -P self-hosted=node:16-buster-slim"

get_env_for() {
  (
    cd ~/Projects/system-mapper
    source venv/bin/activate
    python -m system_mapper --get-env="$1"
  )
}

open_kibana_for() {
  (
    cd ~/Projects/system-mapper
    source venv/bin/activate
    python ./system_mapper/open-service-kibana.py
  )
}


cd .
