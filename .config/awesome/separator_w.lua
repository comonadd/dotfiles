-- File: separator_w.lua
-- Creation date: 2017-02-06
-- Creator: Dmitry Guzeev <dmitry.guzeev@yahoo.com>
-- Description:
-- The separator widget

local m = {}

local wibox = require("wibox")
local config = require("config")

m = wibox.widget.textbox(" | ")
m:set_font(config.theme.SEPARATOR_FONT)

return m
