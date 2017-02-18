urxvtd -o -f -q
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
xset s 0
xset dpms 0 0 1800
exec /usr/bin/awesome
