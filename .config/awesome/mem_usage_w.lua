-- File: mem_usage_w.lua
-- Creation date: 2017-02-06
-- Creator: Dmitry Guzeev <dmitry.guzeev@yahoo.com>
-- Description:

local m = {}

local wibox = require("wibox")
local awful = require("awful")
local config = require("config")

m.widget = wibox.widget.textbox()
m.t = timer({timeout = config.MEM_USAGE_W_TIMEOUT})
m.ram_total = 0
m.swap_total = 0
m.ram_busy = 0
m.swap_busy = 0
m.ram_color = "#FFFFFF"
m.swap_color = "#FFFFFF"
m.init = function ()
   for line in io.lines("/proc/meminfo") do
      local count

      if line:find("MemTotal") then
         line = line:sub(0, -3)
         line, count = line:sub(line:find("MemTotal") - #line + 8):gsub("[^%w]", "")
         m.ram_total = math.ceil(tonumber(line) / 1024)
      elseif line:find("SwapTotal") then
         line = line:sub(0, -3)
         line, count = line:sub(line:find("SwapTotal") - #line + 9):gsub("[^%w]", "")
         m.swap_total = math.ceil(tonumber(line) / 1024)
      end
   end
end

m.update = function ()
   for line in io.lines("/proc/meminfo") do
      local count

      if line:find("MemAvailable") then
         line = line:sub(0, -3)
         line, count = line:sub(line:find("MemAvailable") - #line + 12):gsub("%W", '')
         m.ram_busy = math.ceil(tonumber(line) / 1024)
      elseif line:find("SwapFree") then
         line = line:sub(0, -3)
         line, count = line:sub(line:find("SwapFree") - #line + 8):gsub("%W", '')
         m.swap_busy = math.ceil(tonumber(line) / 1024)
      end
   end
   m.ram_busy = m.ram_total - m.ram_busy
   m.swap_busy = m.swap_total - m.swap_busy

   if m.ram_busy <= m.ram_total - m.ram_total / 2 then
      m.ram_color = config.theme.GREEN_COLOR
   elseif m.ram_busy <= m.ram_total - m.ram_total / 4 then
      m.ram_color = config.theme.YELLOW_COLOR
   else
      m.ram_color = config.theme.RED_COLOR
   end

   if m.swap_busy <= m.swap_total - m.swap_total / 2 then
      m.swap_color = config.theme.GREEN_COLOR
   elseif m.swap_busy <= m.swap_total - m.swap_total / 4 then
      m.swap_color = config.theme.YELLOW_COLOR
   else
      m.swap_color = config.theme.RED_COLOR
   end

   m.widget:set_markup(
      string.format(
         "Mem: <span color='%s'>RAM: %iMB/%iMB</span> # <span color='%s'>SWAP: %uMB/%uMB</span>",
         m.ram_color,
         m.ram_busy,
         m.ram_total,
         m.swap_color,
         m.swap_busy,
         m.swap_total))
end

m.init()
m.update()
m.t:connect_signal("timeout", m.update)
m.t:start()

m.widget:buttons(
   awful.util.table.join(awful.button({}, 1, m.update)))
m.widget:set_font(config.theme.WIBOX_BOT_FONT)


return m
