-- File: username_w.lua
-- Creation date: 2017-02-06
-- Creator: Dmitry Guzeev <dmitry.guzeev@yahoo.com>
-- Description:
-- Username widget

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
