local m = {}

-- Constants
local THEME_DIR = "~/.themes/user"
-- Colors
m.GREEN_COLOR = "#3DC26A"
m.YELLOW_COLOR = "#C6CA35"
m.RED_COLOR = "#CA3535"
m.PURPLE_COLOR = "#7A5ADA"
m.WHITE_COLOR = "#FFFFFF"
m.BLACK_COLOR = "#000000"
m.SHINY_BLACK_COLOR = "#111212"
m.SEPARATOR_COLOR = "#FFFFFF"

-- Fonts
m.SEPARATOR_FONT = "Verdana 14"
m.WIBOX_BOTTOM_FONT = "monospace 8"
m.WIBOX_TOP_FONT = m.WIBOX_BOTTOM_FONT
--}}}

-- Settings
m.wallpaper = THEME_DIR .. "/background.jpg"
m.font = "sans 8"
--}}}

-- Colors
m.fg_normal  = m.WHITE_COLOR
m.fg_focus   = m.PURPLE_COLOR
m.fg_urgent  = m.YELLOW_COLOR
--}}}

m.bg_normal  = m.BLACK_COLOR
m.bg_focus   = m.SHINY_BLACK_COLOR
m.bg_urgent  = m.BLACK_COLOR
m.bg_systray = m.bg_normal
--}}}

return m