-- File: portage_info_w.lua
-- Creation date: 2017-02-06
-- Creator: Dmitry Guzeev <dmitry.guzeev@yahoo.com>
-- Description:

local m = {}

local wibox = require("wibox")
local config = require("config")
local utils = require("utils")

m.widget = wibox.widget.textbox()
m.timer = timer{timeout = config.PORTAGE_INFO_W_TIMEOUT}
m.update = function ()
   local f = io.open("/var/log/emerge.log")
   local status_line

   if not f then
      m.widget:set_markup(
         string.format(
            "Portage: <span color='%s'>Not available</span>",
            config.theme.PURPLE_COLOR))

      return
   end

   f:seek("end", -1024)
   status_line = f:read("*a"):sub(0, -2)
   f:close()
   status_line = status_line:sub(utils.rfind(status_line, "\n"))
   status_line_of = status_line:match("(%d+ of %d+)")

   if status_line:find("Compiling/Merging") then
      status_line = "Compiling/Merging"
   elseif status_line:find("Merging") then
      status_line = "Merging"
   elseif status_line:find("Cleaning") then
      status_line = "Cleaning"
   else
      status_line = "Not installing"
   end

   if status_line_of then
      status_line = string.format(
         "Portage: <span color='%s'>%s</span> (%s)",
         config.theme.PURPLE_COLOR,
         status_line,
         status_line_of)
   else
      status_line = string.format(
         "Portage: <span color='%s'>Not installing</span>",
         config.theme.PURPLE_COLOR,
         status_line)
   end
   m.widget:set_markup(status_line)
end

m.widget:set_font(config.theme.WIBOX_TOP_FONT)
m.update()
m.timer:connect_signal("timeout", m.update)
m.timer:start()

return m
