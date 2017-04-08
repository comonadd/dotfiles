#!/bin/sh
if [[ -n "$1" ]]; then
    setxkbmap $1
else
    layout=$(setxkbmap -query | awk 'END{print $2}')
    echo $layout
    case $layout in
        us)
                setxkbmap ru
            ;;
        *)
                setxkbmap us
            ;;
    esac
fi
