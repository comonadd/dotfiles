#!/usr/bin/env bash

festival --tts $HOME/.config/qtile/welcome_msg &
lxsession &
xcompmgr &
#picom &
nitrogen --restore &
/usr/bin/emacs --daemon &
conky -c $HOME/.config/conky/doomone-qtile.conkyrc
volumeicon &
nm-applet &
ulauncher &

