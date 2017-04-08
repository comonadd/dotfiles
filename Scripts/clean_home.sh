# File: clean_home.sh
# Creation date: 2017-02-08
# Creator: Dmitry Guzeev <dmitry.guzeev@yahoo.com>
# Description:
# The script for cleaning the home (~/) directory

#################
### Histories ###
#################

rm -rf ~/.bash_history ~/.zsh_history ~/.ghc/ghci_history ~/.python_history ~/.lesshst ~/.rediscli_history ~/.gmrun_history ~/.viminfo

####################################
### Cache/Downloads/Temp folders ###
####################################

rm -rf ~/.cache ~/Temp/* ~/Downloads/*
rm -rf ~/.cgdb ~/.wget-hsts ~/.recently-used

#######################
### Log/Error files ###
#######################

rm -rf ~/.awesome-errors
