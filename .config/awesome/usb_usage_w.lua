-- File: usb_usage_w.lua
-- Creation date: 2017-02-06
-- Creator: Dmitry Guzeev <dmitry.guzeev@yahoo.com>
-- Description:
-- USB usage widget

local m = {}

local wibox = require("wibox")
local config = require("config")
local utils = require("utils")

m.widget = wibox.widget.textbox()
m.t = timer({timeout = config.USB_USAGE_W_TIMEOUT})
m.count = 0
m.update = function ()
   local handle = io.popen("lsusb")
   local result = handle:read("*a")

   for index, line in pairs(utils.split_by_newline(result)) do
      if (#line > 0) and (not line:find("Linux Foundation")) then
         m.count = m.count + 1
      end
   end
   m.widget:set_markup(
      string.format(
         "Usb: <span color='%s'>%u</span>",
         config.theme.PURPLE_COLOR,
         m.count))
   m.count = 0
   handle:close()
end

m.widget:set_font(config.theme.WIBOX_BOT_FONT)
m.update()
m.t:connect_signal("timeout", m.update)
m.t:start()

return m
