###
### The .xsession (.xinitrc) file
### Created: 2017/03/03 at 20:03
### Author: wrongway4you
###

xkbcomp $HOME/.config/xkb/my $DISPLAY

#################
### Variables ###
#################

WALLPAPER_PATH=$HOME/.config/wallpaper.jpg

# Update the X configuration
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# Set the default cursor
xsetroot -cursor_name left_ptr

# Set the wallpaper
feh --bg-scale $WALLPAPER_PATH

# Execute the window manager
exec xmonad
