local m = {}

local awful = require("awful")
local utils = require("utils")
local gears = require("gears")

m.theme = require("beautiful")

-- Settings
-- User-specific
m.USERNAME = "wrongway4you"
m.EDITOR = "vim"
m.BROWSER = "google-chrome-stable"
m.TERMINAL = "urxvtc"
m.SCREENSHOTS_FOLDER = "/home/$USER/Screenshots"
m.SCREENSHOT_TOOL = "gnome-screenshot -p -f \"" .. m.SCREENSHOTS_FOLDER ..  "/$(date +%F_%H-%M-%S)_D.png\""
m.GOOGLE_SEARCH_URL = "https://www.google.com.ua/search?q="
m.WIKIPEDIA_WIKI_URL = "https://en.wikipedia.org/wiki/"
m.DEFAULT_SOUND_LEVEL = 80
m.THEME_PATH = "~/.themes/user/theme.lua"
m.LAYOUTS = {
    awful.layout.suit.tile,
    awful.layout.suit.fair,
    awful.layout.suit.max.fullscreen
}
m.ALIASES = {
    term = m.TERMINAL,
    uX = "xrdb ~/.Xresources"
}
m.ALIASES_NAMES = utils.keys(m.ALIASES)
m.SOUND_LEVEL_STEP = 4
-- }}

-- System-specific
m.NETWORK_INTERFACE = "enp2s0"
-- }}

-- MPD info widget
m.MPD_INFO_W_TIMEOUT = 1       -- Update timeout, in seconds
-- }}

-- Memory usage widget
m.MEM_USAGE_W_TIMEOUT = 8      -- Update timout, in seconds
-- }}

-- USB usage widget
m.USB_USAGE_W_TIMEOUT = 16     -- Update timeout, in seconds
-- }}

-- CPU usage widget
m.CPU_USAGE_W_TIMEOUT = 1      -- Udate timeout, in seconds
-- }}

-- Network usage widget
m.NET_USAGE_W_TIMEOUT = 1      -- Update timeout, in seconds
-- }}

-- Disk usage widget
m.DISK_USAGE_W_TIMEOUT = 32    -- Update timeout, in seconds
-- }}

-- Temperature widget
m.TEMP_W_TIMEOUT = 8           -- Update timeout, in seconds
m.TEMP_W_MAX_SYS_TEMP = 60     -- Max temperature
m.TEMP_W_MAX_CPU_TEMP = 80     -- Max temperature
-- }}

-- Portage info widget
m.PORTAGE_INFO_W_TIMEOUT = 32  -- Update timeout, in seconds
-- }}
-- }


m.theme.init(m.THEME_PATH)
gears.wallpaper.maximized(m.theme.wallpaper, 1, true)

return m
