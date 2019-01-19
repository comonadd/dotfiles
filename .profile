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

###################################################
### Other variables
###################################################

# Locale settings
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# ZSH
export ZSH=~/.oh-my-zsh
export UPDATE_ZSH_DAYS=16

# Editor
export EDITOR=vim

# C/C++ Compilers
export CC="/usr/local/opt/llvm/bin/clang"
export CXX="/usr/local/opt/llvm/bin/clang++"

# NVM
export NVM_DIR="$HOME/.nvm"
