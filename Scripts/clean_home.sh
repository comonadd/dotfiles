# File: clean_home.sh
# Creation date: 2017-02-08
# Creator: Dmitry Guzeev <dmitry.guzeev@yahoo.com>
# Description:
# The script for cleaning the home (~/) directory

# Remove "history" files
rm -rf ~/.bash_history ~/.zsh_history ~/.ghc/ghci_history ~/.python_history ~/.lesshst ~/.rediscli_history ~/.gmrun_history ~/.viminfo

# Remove Cache/Downloads/Temp folders
rm -rf ~/.cache ~/Temp/* ~/Downloads/*
rm -rf ~/.cgdb ~/.wget-hsts ~/.recently-used

# Remove log/error files
rm -rf ~/.awesome-errors
