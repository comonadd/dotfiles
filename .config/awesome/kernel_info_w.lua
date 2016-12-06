local m = {}

local wibox = require("wibox")
local config = require("config")
local utils = require("utils")

m = wibox.widget.textbox(
                string.format(
                    "<span color='%s'>%s</span>",
                    config.theme.PURPLE_COLOR,
                    utils.get_uname()))
m:set_font(config.theme.WIBOX_BOT_FONT)

return m
