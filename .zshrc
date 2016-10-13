# Exporting env variables
export PS1='\[\033[01;32m\]\u@\h \[\033[01;34m\]\W \$ \[\033[00m\]'
export ZSH=~/.oh-my-zsh
export LANG=en_US.UTF-8
export UPDATE_ZSH_DAYS=16

# Configuring ZSH
ZSH_THEME="dieter"
DISABLE_AUTO_UPDATE="true"
DISABLE_AUTO_TITLE="true"
ENABLE_CORRECTION="false"
COMPLETION_WAITING_DOTS="true"
DISABLE_UNTRACKED_FILES_DIRTY="true"
plugins=(git)

# Initializing
source $ZSH/oh-my-zsh.sh
source ~/Scripts/motd.sh

# Change directory aliases
alias cdG="cd ~/Git/dotfiles"


# Short-cut aliases
alias ll="ls -l"
alias la="ls -la"

# "Update" aliases
alias uX="xrdb ~/.Xresources"

# "Edit" aliases
alias eA="cd ~/Git/dotfiles/.config/awesome && vim rc.lua"
alias eZ="cd ~/Git/dotfiles && vim .zshrc"
alias eV="cd ~/Git/dotfiles && vim .vimrc"
