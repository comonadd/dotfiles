#!/usr/local/bin/fish

# fish_vi_key_bindings
fish_default_key_bindings

# Git abbreviations
abbr gita "git add"
abbr gitc "git commit"
abbr gitl "git log"
abbr gitm "git merge"
abbr gitp "git pull"
abbr gitps "git push"
abbr gits "git status"
abbr find "fd"

# Docker abbreviations
abbr docker-ls-all-containers "docker ps -aq"
abbr docker-stop-all-containers "docker stop (docker ps -aq)"
abbr docker-rm-all-containers "docker rm \$(docker ps -aq)"
abbr docker-rm-all-images "docker rmi \$(docker images -q)"
function docker-run-mysql-dev-container
  echo "Starting MySQL development database Docker container";
	cd ~/Docker/local-mysql-db && docker-compose up
end
function docker-run-rabbitmq-dev-container
  echo "Starting RabbitMQ development Docker container";
	cd ~/Docker/local-rabbitmq && docker-compose up
end
function docker-run-postgres-dev-container
  echo "Starting PostgreSQL development database Docker container";
	cd ~/Docker/local-postgres-db && docker-compose up
end
function docker-run-redis-dev-container
  echo "Starting Redis development database Docker container";
	cd ~/Docker/local-redis-db && docker-compose up
end
function docker-run-mongo-dev-container
  echo "Starting MongoDB development Docker container";
	cd ~/Docker/local-mongo-db && docker-compose up
end

# Internet
abbr yt "youtube-dl --add-metadata -ic"
abbr yta "youtube-dl --add-metadata -xic --audio-format mp3 --audio-quality 0"

# CD
abbr -a -- - 'cd -'

# Brew
function brew-cleanup
  brew cleanup
end
function brew-upgrade
  brew upgrade
end

# Cleanup
function kill-adobe
  pgrep -i adobe | sudo xargs kill -9
  pgrep -i "Core Sync" | sudo xargs kill -9
  pgrep -i "ACCFinderSync" | sudo xargs kill -9
  pgrep -i "AGSService" | sudo xargs kill -9
  pgrep -i "AGMService" | sudo xargs kill -9
end
function cleanup
  printf "Killing all adobe processes..."
  kill-adobe
  printf "Done\n"
  printf "Cleaning up brewmaster packages..."
  brew-cleanup
  printf "Done\n"
end

# System
abbr restart-touchbar "pgrep -i touchbar | xargs sudo kill -9"
abbr kill-all-node-processes "pgrep -i node | xargs kill -9"
abbr kill-all-python-processes "pgrep -i python | xargs kill -9"
function kill-port-user
  lsof -t -i tcp:$argv | xargs kill
end

# Other
abbr ka "killall"
abbr lsl "sudo cat /var/log/system.log"
abbr mkd "mkdir -pv"
abbr r "ranger"
abbr ls "exa"
abbr la "exa -la"
abbr ll "exa -l"
abbr transform-all-flv-to-mp4 "python -c \"import os; [os.system('ffmpeg -i \'{a}\' -c:v libx264 -crf 19 -strict experimental \'{b}\''.format(a=f, b=f.replace('.flv', '.mp4'))) if f.endswith('.flv') else '' for f in os.listdir('./')]\""
abbr transform-all-webm-to-mp4 "python -c \"import os; [os.system('ffmpeg -fflags +genpts -i \'{a}\' -r 24 \'{b}\''.format(a=f, b=f.replace('.webm', '.mp4'))) if f.endswith('.webm') else '' for f in os.listdir('./')]\""
abbr clear-fish-history "builtin history clear"


# Use .profile defined PATH modifications
egrep "^export " ~/.profile | while read e
    set var (echo $e | sed -E "s/^export ([A-Z_]+)=(.*)\$/\1/")
    set value (echo $e | sed -E "s/^export ([A-Z_]+)=(.*)\$/\2/")

    # remove surrounding quotes if existing
    set value (echo $value | sed -E "s/^\"(.*)\"\$/\1/")

    if test $var = "PATH"
        # replace ":" by spaces. this is how PATH looks for Fish
        set value (echo $value | sed -E "s/:/ /g")

        # use eval because we need to expand the value
        eval set -xg $var $value

        continue
    end

    # evaluate variables. we can use eval because we most likely just used "$var"
    set value (eval echo $value)

    set -xg $var $value
end

# Had to comment this out because Emacs doesn't process this very well
#stty -ixon

export HISTSIZE=
export HISTFILESIZE=

##################################
### AUTOENV
##################################

if [ -z $AUTOENV_AUTH_FILE ]
  set AUTOENV_AUTH_FILE "$HOME/.autoenv_authorized"
end

function _autoenv_exec
  if [ ! -f $argv ]
    return
  end

  if which shasum > /dev/null ^ /dev/null
    set hash (shasum $argv | cut -d' ' -f 1)
  else
    set hash (sha1sum $argv | cut -d' ' -f 1)
  end

  if grep "$argv:$hash" "$AUTOENV_AUTH_FILE"  > /dev/null ^ /dev/null
    . $argv
  else
    echo "> WARNING"
    echo "> This is the first time you are about to source \"$argv\""
    echo
    echo "----------------"
    echo
    cat $argv
    echo
    echo "----------------"
    echo
    echo -n "Are you sure you want to allow this? (y/N)"

    read answer

    if [ $answer = "y" -o $answer = "Y" ]
      echo "$argv:$hash" >> $AUTOENV_AUTH_FILE
      . $argv
    end
  end
end

function _autoenv_explodepath
  set curpath $argv

  if [ -z $curpath ]
    set curpath "/"
  end

  while [ ! $curpath = "/" ]
    echo $curpath
    set curpath (dirname $curpath)
  end

  echo "/"
end

function _autoenv --on-variable PWD
  if status --is-command-substitution
    return
  end

  set -l newpath $PWD

  if [ -z $AUTOENV_OLDPATH ]
    _autoenv_exec "/.env.fish"
    set -g AUTOENV_OLDPATH "/"
  end

  if [ $newpath = $AUTOENV_OLDPATH ]
    return
  end

  set newpath (_autoenv_explodepath $newpath)[-1..1]
  set AUTOENV_OLDPATH (_autoenv_explodepath $AUTOENV_OLDPATH)[-1..1]

  set -l commonindex 1

  for i in (seq (count $newpath))
    if [ (count $AUTOENV_OLDPATH) -lt $i ]
      break
    end

    if [ $newpath[$i] = $AUTOENV_OLDPATH[$i] ]
      set commonindex $i
    else
      break
    end
  end

  if [ $commonindex -lt (count $AUTOENV_OLDPATH) ]
    for op in $AUTOENV_OLDPATH[-1..(math $commonindex+1)]
      _autoenv_exec $op/.out.fish
    end
  end

  if [ $commonindex -lt (count $newpath) ]
    for op in $newpath[(math $commonindex+1)..-1]
      _autoenv_exec $op/.env.fish
    end
  end

  set -g AUTOENV_OLDPATH $PWD
end

