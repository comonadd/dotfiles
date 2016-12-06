local m = {}

local wibox = require("wibox")
local config = require("config")


m.widget= wibox.widget.textbox()
m.current = 1
m.layout = {
    {"us", "US"},
    {"ru", "RU"}
}
m.switch = function ()
    local t
    m.current = m.current % #(m.layout) + 1
    t = m.layout[m.current]
    m.widget:set_text(t[2])
    os.execute("setxkbmap " .. t[1])
end

m.widget:set_font(config.theme.WIBOX_TOP_FONT)
m.widget:set_text(m.layout[m.current][2])

return m
