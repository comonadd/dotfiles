local m = {}

local wibox = require("wibox")
local config = require("config")


m = wibox.widget.textbox(
                string.format(
                    "<span color='%s'>%s</span>",
                    config.theme.PURPLE_COLOR,
                    config.USERNAME))
m:set_font(config.theme.WIBOX_BOT_FONT)

return m
