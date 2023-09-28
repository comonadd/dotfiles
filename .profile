# This file contains env & path configuration

IS_MACOS=$(uname -a | grep Darwin)

# Python startup
if [ -f ~/.pystartup.py ]; then
    export PYTHONSTARTUP=~/.pystartup.py
fi

# Locale settings
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# ZSH
export ZSH=~/.oh-my-zsh
export UPDATE_ZSH_DAYS=16

# Editor
export EDITOR=code

# C/C++ Compilers
export CC="/usr/local/opt/llvm/bin/clang"
export CXX="/usr/local/opt/llvm/bin/clang++"

# NVM
export NVM_DIR="$HOME/.nvm"

# Set CLICOLOR if you want Ansi Colors in iTerm2
export CLICOLOR=1

# Set colors to match iTerm2 Terminal Colors
export TERM=xterm-256color

if [[ $IS_MACOS ]]; then
    export C_INCLUDE_PATH=$C_INCLUDE_PATH:/opt/homebrew/Cellar/librdkafka/2.0.2/includde
    export LIBRARY_PATH=$LIBRARY_PATH:/opt/homebrew/Cellar/librdkafka/2.0.2/lib
    export C_INCLUDE_PATH=$C_INCLUDE_PATH:/opt/homebrew/include
    export LIBRARY_PATH=$LIBRARY_PATH:/opt/homebrew/lib
fi

export EDITOR="vim"
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:$HOME/go/bin"
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:$HOME/Library/Python/3.8/bin"
export PATH="$HOME/.pyenv/shims:${PATH}"

# Ruby gems
export PATH=$PATH:$HOME/.gem/ruby/2.4.0/bin

# Rust crates
export PATH=$PATH:$HOME/.cargo/bin

# Haskell packages
export PATH=$PATH:$HOME/.cabal/bin

# Custom scripts
export PATH=$PATH:$HOME/Scripts
export PATH=$PATH:$HOME/Scripts/android
export PATH=$PATH:$HOME/Scripts/macos
export PATH=$PATH:$HOME/Scripts/python-projects

# System folders
export PATH=$PATH:/opt/local/bin

# Go
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH:$GOPATH/bin

# Anaconda3
export PATH="$HOME/anaconda3/bin:$PATH"

# Clang
export PATH="$PATH:/usr/local/opt/llvm/bin"

# Local node_modules
export PATH="$PATH:./node_modules/.bin"

# Local virtualenv (python) modules
export PATH="$PATH:./venv/bin"

# Make
export PATH="/usr/local/opt/make/libexec/gnubin:$PATH"

# Python bins
export PATH="$PATH:$HOME/.asdf/installs/python/3.6.0/bin"

export PATH="$PATH:/usr/local/opt/llvm/bin"

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

export LANG=en_US.UTF-8

export PATH="/opt/homebrew/opt/ruby@3.1/bin:$PATH"

export PATH="/opt/homebrew/opt/openssl@3/bin:$PATH"

# fnm
export PATH=/home/comonadd/.fnm:$PATH

export PATH="$PATH:/Users/comonadd/.local/bin"