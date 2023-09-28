source ~/.config/.bashrc.common
source ~/.profile

ZSH_THEME="agnoster"

force_color_prompt=yes

zstyle ':omz:update' mode auto      # update automatically without asking
zstyle ':omz:update' frequency 13

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

[[ $commands[kubectl] ]] && source <(kubectl completion zsh)

bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# Prompt
PROMPT='%B%F{240}%1~%f%b %# '

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

export CC=/usr/bin/clang
export CXX=/usr/bin/clang++

eval "$(fnm env --use-on-cd)"
