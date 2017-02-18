-- File: panel_bot.lua
-- Creation date: 2017-02-08
-- Creator: Dmitry Guzeev <dmitry.guzeev@yahoo.com>
-- Description:
-- Bottom panel

local awful = require("awful")
local wibox = require("wibox")

local utils = require("utils")
local config = require("config")
local sound_manager = require("sound_manager")
local separator_w = require("separator_w")
local portage_info_w = require("portage_info_w")
local net_usage_w = require("net_usage_w")
local cpu_usage_w = require("cpu_usage_w")
local mpd_info_w = require("mpd_info_w")
local cli_w = require("cli_w")
local datetime_w = require("datetime_w")
local uptime_w = require("uptime_w")
local kbl_w = require("kbl_w")
local mem_usage_w = require("mem_usage_w")
local temp_w = require("temp_w")
local usb_usage_w = require("usb_usage_w")
local disk_usage_w = require("disk_usage_w")
local kernel_info_w = require("kernel_info_w")
local username_w = require("username_w")

----------------------
-- The actual panel --
----------------------

local panel_bot = awful.wibox({
      position = "bottom",
      screen = 1,
      height = config.theme.PANEL_BOT_H,
      bg = config.theme.bg_normal,
      fg = config.theme.fg_normal
})

---------------
-- Constants --
---------------

local END_SEP = "  "

---------------------
-- The left layout --
---------------------

local panel_bot_layout_left = wibox.layout.fixed.horizontal()
panel_bot_layout_left.widgets = {
   wibox.widget.textbox(END_SEP),
   mem_usage_w.widget,
   separator_w, temp_w.widget,
   separator_w, disk_usage_w.widget,
   separator_w, usb_usage_w.widget,
   separator_w
}

-----------------------
-- The middle layout --
-----------------------

local panel_bot_layout_middle = wibox.layout.fixed.horizontal()
panel_bot_layout_middle.widgets = {
   mpd_info_w.widget
}

----------------------
-- The right layout --
----------------------

local panel_bot_layout_right = wibox.layout.fixed.horizontal()
panel_bot_layout_right.widgets = {
   separator_w,
   username_w, separator_w,
   kernel_info_w, separator_w,
   uptime_w.widget, wibox.widget.textbox(END_SEP)
}

---------------------
-- The main layout --
---------------------

local panel_bot_layout_top = wibox.layout.align.horizontal()
panel_bot_layout_top:set_left(panel_bot_layout_left)
panel_bot_layout_top:set_middle(panel_bot_layout_middle)
panel_bot_layout_top:set_right(panel_bot_layout_right)
panel_bot:set_widget(panel_bot_layout_top)
