# File: weather.sh
# Creation date: 2017-03-03
# Creator: Dmitry Guzeev <dmitry.guzeev@yahoo.com>
# Description:

WIN_MANAGER_CMD=xmonad

# Load the XKB settings
$HOME/Scripts/load_xkb_settings.sh

# Set the default cursor
xsetroot -cursor_name left_ptr

# Update the X configuration
$HOME/Scripts/update_x_config.sh

# Set the wallpaper
$HOME/Scripts/set_wallpaper.sh

# Execute the window manager
exec $WIN_MANAGER_CMD
