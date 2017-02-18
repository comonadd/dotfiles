-- File: panel_top.lua
-- Creation date: 2017-02-08
-- Creator: Dmitry Guzeev <dmitry.guzeev@yahoo.com>
-- Description:
-- Top panel

local awful = require("awful")
local wibox = require("wibox")
awful.rules = require("awful.rules")
require("awful.autofocus")

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
local tag_list = require("tag_list")

local wibox_top = awful.wibox({
      position = "top",
      screen = 1,
      height = config.theme.WIBOX_TOP_H,
      bg = config.theme.bg_normal,
      fg = config.theme.fg_normal
})

local wibox_tl_l = wibox.layout.fixed.horizontal()
wibox_tl_l:add(tag_list.widget)
wibox_tl_l:add(separator_w)
wibox_tl_l:add(cli_w.widget)

local wibox_tr_l = wibox.layout.fixed.horizontal()
wibox_tr_l:add(separator_w)
wibox_tr_l:add(portage_info_w.widget)
wibox_tr_l:add(separator_w)
wibox_tr_l:add(net_usage_w.widget)
wibox_tr_l:add(separator_w)
wibox_tr_l:add(cpu_usage_w.widget)
wibox_tr_l:add(separator_w)
wibox_tr_l:add(kbl_w.widget)
wibox_tr_l:add(separator_w)
wibox_tr_l:add(datetime_w.date_widget)
wibox_tr_l:add(separator_w)
wibox_tr_l:add(datetime_w.time_widget)
wibox_tr_l:add(wibox.widget.textbox("  "))

local wibox_top_l = wibox.layout.align.horizontal()
wibox_top_l:set_left(wibox_tl_l)
wibox_top_l:set_right(wibox_tr_l)
wibox_top:set_widget(wibox_top_l)
