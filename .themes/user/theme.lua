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

-- Separator
m.SEPARATOR_FONT = "Sans 10"
--}}}

-- Settings
m.wallpaper = THEME_DIR .. "/background.jpg"
m.font = "Sans 8"
--}}}


-- Wibox top
m.WIBOX_TOP_H = 20
m.WIBOX_TOP_FONT = "Sans 7"
m.WIBOX_TOP_BG = m.BLACK_COLOR
m.WIBOX_TOP_FG = m.WHITE_COLOR
--}}}

-- Wibox bot
m.WIBOX_BOT_H = m.WIBOX_TOP_H
m.WIBOX_BOT_FONT = m.WIBOX_TOP_FONT
m.WIBOX_BOT_BG = m.WIBOX_TOP_BG
m.WIBOX_BOT_FG = m.WIBOX_TOP_FG
--}}}

-- Tag list
m.TAG_LIST_FG_FOCUS = m.PURPLE_COLOR
m.TAG_LIST_BG_FOCUS = m.SHINY_BLACK_COLOR
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
