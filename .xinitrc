urxvtd -o -f -q
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

exec awesome
