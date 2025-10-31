
eval "$(/opt/homebrew/bin/brew shellenv)"

export PATH="/Users/guzeev/.local/state/fnm_multishells/11576_1741017194973/bin":$PATH
export FNM_MULTISHELL_PATH="/Users/guzeev/.local/state/fnm_multishells/11576_1741017194973"
export FNM_VERSION_FILE_STRATEGY="local"
export FNM_DIR="/Users/guzeev/.local/share/fnm"
export FNM_LOGLEVEL="info"
export FNM_NODE_DIST_MIRROR="https://nodejs.org/dist"
export FNM_COREPACK_ENABLED="false"
export FNM_RESOLVE_ENGINES="true"
export FNM_ARCH="arm64"
rehash

# Setting PATH for Python 3.12
# The original version is saved in .zprofile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.12/bin:${PATH}"
export PATH
