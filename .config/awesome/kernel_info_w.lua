-- File: kernel_info_w.lua
-- Creation date: 2017-02-06
-- Creator: Dmitry Guzeev <dmitry.guzeev@yahoo.com>
-- Description:

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
