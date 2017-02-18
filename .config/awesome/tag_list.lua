-- File: tag_list.lua
-- Creation date: 2017-02-08
-- Creator: Dmitry Guzeev <dmitry.guzeev@yahoo.com>
-- Description:
-- Tag list

local awful = require("awful")
local wibox = require("wibox")

local config = require("config")

local m = {}

m.tags = awful.tag(config.TAGS, 1, config.LAYOUTS[1])

m.tag_list_buttons = awful.util.table.join(
   awful.button({}, 1, awful.tag.viewonly))

m.tag_list = awful.widget.taglist(
   1,
   function ()
      return m.tags
   end,
   m.tag_list_buttons,
   {font = config.theme.WIBOX_TOP_FONT})

m.tag_list.base_widget = wibox.layout.fixed.horizontal()

m.widget = m.tag_list

return m
