###################################################
### PATH
###################################################

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
export PATH="/Users/wrongway4you/anaconda3/bin:$PATH"

# Clang
export PATH="$PATH:/usr/local/opt/llvm/bin"

# Local node_modules
export PATH="$PATH:./node_modules/.bin"

# Local virtualenv (python) modules
export PATH="$PATH:./venv/bin"

# Make
export PATH="/usr/local/opt/make/libexec/gnubin:$PATH"

# Python bins
export PATH="$PATH:/Users/wrongway4you/.asdf/installs/python/3.6.0/bin"

export PATH="$PATH:/usr/local/opt/llvm/bin"

###################################################
### Other variables
###################################################

# Python startup
PYTHONSTARTUP=~/.pystartup.py

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
