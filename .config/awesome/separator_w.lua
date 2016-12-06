local m = {}

local wibox = require("wibox")
local config = require("config")


m = wibox.widget.textbox(" | ")
m:set_font(config.theme.SEPARATOR_FONT)

return m
