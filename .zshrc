###################################################
### Setup environment variables
###################################################

source ~/.profile

###################################################
### Define needed functions
###################################################

# Pull all remote Git branches
pull-all-git-branches() {
    git branch -r | grep -v "\->" | while read remote; do git branch --track "${remote#origin/}" "$remote"; done
    git fetch --all
    git pull --all
    for remote in `git branch -r`; do git branch --track ${remote#origin/} $remote; done
}

###################################################
### Initialize oh-my-zsh
###################################################

# Theme
ZSH_THEME="bira"

# Other options
CASE_SENSITIVE="true"
HYPHEN_INSENSITIVE="false"
DISABLE_AUTO_UPDATE="true"
DISABLE_LS_COLORS="false"
DISABLE_AUTO_TITLE="true"
ENABLE_CORRECTION="false"
COMPLETION_WAITING_DOTS="false"
DISABLE_UNTRACKED_FILES_DIRTY="true"
HIST_STAMPS="mm/dd/yyyy"

# Plugins
plugins=(osx)

# Execute main oh-my-zsh script
source $ZSH/oh-my-zsh.sh

# Enable Vim mode
bindkey -v

# Only check local files in Git plugin
__git_files () {
    _wanted files expl "local files" _files
}

autoload -U add-zsh-hook

update-terminal-pwd-title() {
  echo -ne "\033]0;$PWD\007"
}

add-zsh-hook chpwd update-terminal-pwd-title
update-terminal-pwd-title

###################################################
### Setup terminal options
###################################################

unsetopt MULTIOS
setopt MAGIC_EQUAL_SUBST
setopt BSD_ECHO
setopt INTERACTIVE_COMMENTS
setopt PROMPT_SUBST

###################################################
### Fix problems with dumb terminals
###################################################

[[ $TERM == "dumb" ]] && unsetopt zle && PS1="$ " && return

###################################################
### Initialize NVM
###################################################

USE_FAST_NVM_LOAD_METHOD="1"

if [ $USE_FAST_NVM_LOAD_METHOD -eq "1" ]; then
  declare -a NODE_GLOBALS=(`find ~/.nvm/versions/node -maxdepth 3 -type l -wholename '*/bin/*' | xargs -n1 basename | sort | uniq`)
  NODE_GLOBALS+=("node")
  NODE_GLOBALS+=("nvm")
  load_nvm () {
      [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
  }
  for cmd in "${NODE_GLOBALS[@]}"; do
      eval "${cmd}(){ unset -f ${NODE_GLOBALS}; load_nvm; ${cmd} \$@ }"
  done
  check-for-new-nvmrc-configs() {
    if [[ -a .nvmrc ]]; then
      nvm use
    fi
  }
  add-zsh-hook chpwd check-for-new-nvmrc-configs
  check-for-new-nvmrc-configs
else
  [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" --no-use
  [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
  # Load local .nvmrc right after cd-ing into the directory
  load-nvmrc() {
    local node_version="$(nvm version)"
    local nvmrc_path="$(nvm_find_nvmrc)"
    if [ -n "$nvmrc_path" ]; then
      echo "Loading nvmrc: Found NVM config file at \"$nvmrc_path\""
      local nvmrc_node_version=$(nvm version "$(cat "${nvmrc_path}")")
      if [ "$nvmrc_node_version" = "N/A" ]; then
        nvm install
      elif [ "$nvmrc_node_version" != "$node_version" ]; then
        nvm use
      fi
    elif [ "$node_version" != "$(nvm version default)" ]; then
      echo "Loading nvmrc: Reverting to nvm default version"
      nvm use default
    else
      echo "Did not do anything"
    fi
  }
  add-zsh-hook chpwd load-nvmrc
  load-nvmrc
fi

###################################################
### Setup autoenv ZSh plugin
###################################################

export AUTOENV_FILE_ENTER=.autoenv.zsh
export AUTOENV_FILE_LEAVE=.autoenv.zsh
export AUTOENV_HANDLE_LEAVE=1
source ~/.oh-my-zsh/plugins/zsh-autoenv/autoenv.zsh

###################################################
### Setup command aliases
###################################################

# EDIT aliases
alias eZ="$EDITOR ~/.zshrc"
alias eV="$EDITOR ~/.vimrc"

# Clipboard management aliases
alias cclip="xclip -selection clipboard"
alias clipp="xclip -selection clipboard -o"

# Command flag modification aliases
alias make="make --warn-undefined-variables"

# Git aliases
alias gits="git status"
alias gitc="git commit"
alias gitpab=pull-all-git-branches

# Compilers
alias clang="clang-7"
alias clang++="clang-7"

# Other aliases
alias uX="xrdb ~/.Xresources"
alias rr="clear && printf \"\e[3J\""
alias vi="vim"
alias neovim="nvim"
alias vim="neovim"
alias pip="pip3"
alias python="python3"
alias gradlew="gradle wrapper"
alias find="fd"
alias php="/usr/local/bin/php"
alias pyenv-activate="source ./venv/bin/activate"

# Docker
alias docker-ls-all-containers="docker ps -aq"
alias docker-stop-all-containers="docker stop \$(docker ps -aq)"
alias docker-rm-all-containers="docker rm \$(docker ps -aq)"
alias docker-rm-all-images="docker rmi \$(docker images -q)"

function docker-run-mysql-dev-container() {
  echo "Starting MySQL development database";
	cd ~/Docker/local-mysql-db && docker-compose up
}

function docker-run-redis-dev-container() {
  echo "Starting Redis development database";
	cd ~/Docker/local-redis-db && docker-compose up
}
