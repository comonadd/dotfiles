-- File: uptime_w.lua
-- Creation date: 2017-02-06
-- Creator: Dmitry Guzeev <dmitry.guzeev@yahoo.com>
-- Description:
-- Uptime widget

local m = {}

local wibox = require("wibox")
local config = require("config")

m.widget = wibox.widget.textbox()
m.t = timer({timeout = 1})
m.uptime = {}
m.update = function ()
   local file = io.open("/proc/uptime", "r")

   m.uptime.seconds = file:read()
   m.uptime.seconds = tonumber(
      m.uptime.seconds:sub(
         0,
         m.uptime.seconds:find(" ")))
   m.uptime.hours = math.floor(
      m.uptime.seconds / 3600)
   m.uptime.minutes = math.floor(
      (m.uptime.seconds - m.uptime.hours * 3600) / 60)
   m.widget:set_markup(
      string.format(
         "Uptime: <span color='%s'>%.2i:%.2i</span>",
         config.theme.PURPLE_COLOR,
         m.uptime.hours,
         m.uptime.minutes))
   file:close()
end

m.widget:set_font(config.theme.WIBOX_BOT_FONT)
m.update()
m.t:connect_signal("timeout", m.update)
m.t:start()

return m
