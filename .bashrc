# Git aliases
alias gita="git add"
alias gitc="git commit"
alias gitl="git log"
alias gitm="git merge"
alias gitp="git pull"
alias gitps="git push"
alias gits="git status"
alias find="fd"

# LS
alias la="ls -la"

# Docker aliases
alias docker-ls-all-containers="docker ps -aq"
alias docker-stop-all-containers="docker stop (docker ps -aq)"
alias docker-rm-all-containers="docker rm \$(docker ps -aq)"
alias docker-rm-all-images="docker rmi \$(docker images -q)"
function docker-run-mysql-dev-container () {
  echo "Starting MySQL development database Docker container";
  cd ~/Docker/local-mysql-db && docker-compose up
}
function docker-run-rabbitmq-dev-container () {
  echo "Starting RabbitMQ development Docker container";
  cd ~/Docker/local-rabbitmq && docker-compose up
}
function docker-run-postgres-dev-container () {
  echo "Starting PostgreSQL development database Docker container";
  cd ~/Docker/local-postgres-db && docker-compose up
}
function docker-run-redis-dev-container () {
  echo "Starting Redis development database Docker container";
  cd ~/Docker/local-redis-db && docker-compose up
}
function docker-run-mongo-dev-container () {
  echo "Starting MongoDB development Docker container";
  cd ~/Docker/local-mongo-db && docker-compose up
}

# Editor
export EDITOR="emacs"

# Internet
alias yt="youtube-dl --add-metadata -ic"
alias yta="youtube-dl --add-metadata -xic --audio-format mp3 --audio-quality 0"

alias killbg="kill $(jobs -p)"

eval "$(starship init bash)"
