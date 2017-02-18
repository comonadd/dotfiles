-- File: keybindings.lua
-- Creation date: 2017-02-08
-- Creator: Dmitry Guzeev <dmitry.guzeev@yahoo.com>
-- Description:
-- Keybindings for the Awesome WM

local awful = require("awful")

local config = require("config")
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
local sound_manager = require("sound_manager")
local tag_list = require("tag_list")

globalkeys = awful.util.table.join(
   awful.key({"Mod4", "Control"}, "m", sound_manager.mute),

   awful.key({"Mod4", "Control"}, "p", mpd_info_w.pause),

   awful.key({"Mod4", "Control"}, "=",
      function () sound_manager.set_sound_level_relative(config.SOUND_LEVEL_STEP) end),

   awful.key({"Mod4", "Control"}, "-",
      function () sound_manager.set_sound_level_relative(-config.SOUND_LEVEL_STEP) end),

   awful.key({"Mod4"}, "p", cli_w.show),
   awful.key({"Mod4"}, "r", awesome.restart),
   awful.key({"Mod4"}, "k", awesome.quit),
   awful.key({"Mod4"}, "a", function () awful.layout.inc(config.LAYOUTS, -1) end),
   awful.key({"Mod4"}, "d", function () awful.layout.inc(config.LAYOUTS, 1) end),
   awful.key({"Mod4"}, "j", function () utils.run_cmd(config.SCREENSHOT_TOOL) end),

   awful.key({"Mod4"}, "Left", function () awful.tag.viewprev() end),
   awful.key({"Mod4"}, "Right", function () awful.tag.viewnext() end),

   awful.key({"Mod4"}, "q",
      function ()
         awful.client.focus.byidx(-1)
         if client.focus then client.focus:raise() end end),

   awful.key({"Mod4"}, "e",
      function ()
         awful.client.focus.byidx(1)
         if client.focus then client.focus:raise() end end),

   awful.key({"Mod4"}, "t", function () awful.util.spawn(config.TERMINAL) end),
   awful.key({"Mod4"}, "=", function () awful.tag.incmwfact(-0.05) end),
   awful.key({"Mod4"}, "-", function () awful.tag.incmwfact(0.05) end),
   awful.key({"Mod1"}, "Shift_L", kbl_w.switch),
   awful.key({"Shift"}, "Alt_L", kbl_w.switch))

clientkeys = awful.util.table.join(
   awful.key({"Mod4"}, "c", function (c) c:kill() end),
   awful.key({"Mod4"}, "s", function (c) c:swap(awful.client.getmaster()) end),
   awful.key({"Mod4"}, "f", function (c) c.fullscreen = not c.fullscreen  end),
   awful.key({"Mod4"}, "z", function (c) c.minimized = true end),
   awful.key({"Mod4"}, "x", function (c) c.minimized = false end),
   awful.key({"Mod4"}, "m", function (c)
         c.maximized_horizontal = not c.maximized_horizontal
         c.maximized_vertical = not c.maximized_vertical end))

clientbuttons = awful.util.table.join(
   awful.button({}, 1, function (c) client.focus = c; c:raise() end))

for i = 1, 8 do
   globalkeys = awful.util.table.join(
      globalkeys,

      awful.key({"Mod4"}, "#" .. i + 9,
         function ()
            local screen = mouse.screen
            local tag = awful.tag.gettags(screen)[i]
            if tag then awful.tag.viewonly(tag) end
      end),

      awful.key({"Mod4", "Control"}, "#" .. i + 9,
         function ()
            local screen = mouse.screen
            local tag = awful.tag.gettags(screen)[i]
            if tag then awful.tag.viewtoggle(tag) end
      end),

      awful.key({"Mod4", "Shift"}, "#" .. i + 9,
         function ()
            local screen = mouse.screen
            local tag = awful.tag.gettags(screen)[i]
            if client.focus and awful.tag.gettags(client.focus.screen)[i] then
               awful.client.movetotag(tag)
            end
   end))
end
root.keys(globalkeys)
