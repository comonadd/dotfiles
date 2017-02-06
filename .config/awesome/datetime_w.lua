-- File: datetime_w.lua
-- Creation date: 2017-02-06
-- Creator: Dmitry Guzeev <dmitry.guzeev@yahoo.com>
-- Description:

local m = {}

local wibox = require("wibox")
local config = require("config")

m.time_widget = wibox.widget.textbox()
m.date_widget = wibox.widget.textbox()
m.t = timer({timeout = 60})
m.update = function ()
   m.time_widget:set_text(os.date("%H:%M"))
   m.date_widget:set_text(os.date("%d/%m"))
end

m.time_widget:set_font(config.theme.WIBOX_TOP_FONT)
m.date_widget:set_font(config.theme.WIBOX_TOP_FONT)
m.update()
m.t:connect_signal("timeout", m.update)
m.t:start()

return m
