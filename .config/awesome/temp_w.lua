-- File: temp_w.lua
-- Creation date: 2017-02-06
-- Creator: Dmitry Guzeev <dmitry.guzeev@yahoo.com>
-- Description:
-- Temperature widget

local m = {}

local wibox = require("wibox")
local config = require("config")

m.widget = wibox.widget.textbox()
m.t = timer({timeout = config.TEMP_W_TIMEOUT})
m.sys_temp = 0
m.cpu_temp = 0
m.sys_temp_max = 0
m.cpu_temp_max = 0
m.sys_color = "#FFFFFF"
m.cpu_color = "#FFFFFF"
m.init = function ()
   local file

   file = io.open("/sys/class/hwmon/hwmon1/temp1_max")
   if file then
      m.sys_temp_max = tonumber(file:read()) / 1000
      file:close()
   else
      m.sys_temp_max = config.TEMP_W_MAX_SYS_TEMP
   end

   file = io.open("/sys/class/hwmon/hwmon1/temp2_max")
   if file then
      m.cpu_temp_max = tonumber(file:read()) / 1000
      file:close()
   else
      m.cpu_temp_max = config.TEMP_W_MAX_CPU_TEMP
   end
end

m.update = function ()
   local file

   file = io.open("/sys/class/hwmon/hwmon1/temp1_input")
   if file then
      m.sys_temp = tonumber(file:read()) / 1000
      file:close()
   else
      m.sys_temp = -1
   end

   file = io.open("/sys/class/hwmon/hwmon1/temp2_input")
   if file then
      m.cpu_temp = tonumber(file:read()) / 1000
      file:close()
   else
      m.cpu_temp = -1
   end

   if m.sys_temp <= (m.sys_temp_max - (m.sys_temp_max / 2)) then
      m.sys_color = config.theme.GREEN_COLOR
   elseif m.sys_temp <= (m.sys_temp_max - (m.sys_temp_max / 4)) then
      m.sys_color = config.theme.YELLOW_COLOR
   else
      m.sys_color = config.theme.RED_COLOR
   end

   if m.cpu_temp <= (m.cpu_temp_max - (m.cpu_temp_max / 2)) then
      m.cpu_color = config.theme.GREEN_COLOR
   elseif m.cpu_temp <= (m.cpu_temp_max - (m.cpu_temp_max / 4)) then
      m.cpu_color = config.theme.YELLOW_COLOR
   else
      m.cpu_color = config.theme.RED_COLOR
   end

   m.widget:set_markup(
      string.format(
         "Temp: <span color='%s'>SYSTIN: %.1f°C</span> # <span color='%s'>CPUTIN: %.1f°C</span>",
         m.sys_color,
         m.sys_temp,
         m.cpu_color,
         m.cpu_temp))
end

m.widget:set_font(config.theme.WIBOX_BOT_FONT)
m.init()
m.update()
m.t:connect_signal("timeout", m.update)
m.t:start()

return m
