#!/bin/bash
if [ -f /tmp/keyboard_light ]; then
xset -led 3 && rm /tmp/keyboard_light
else
xset led 3 && touch /tmp/keyboard_light
fi
