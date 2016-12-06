local m = {}

local wibox = require("wibox")
local config = require("config")

m.widget = wibox.widget.textbox()
m.t = timer({timeout = config.CPU_USAGE_W_TIMEOUT})
m.jiffies = {}
m.update = function ()
    local s = ""
    local tmp = ""

    for line in io.lines("/proc/stat") do
        local cpu, newjiffies = string.match(line, "(cpu%d*)\ +(%d+)")

        if cpu and newjiffies then
            if not m.jiffies[cpu] then
                m.jiffies[cpu] = newjiffies
            end
            if s:len() > 0 then
                tmp = string.upper("  " .. cpu)
            else
                tmp = string.upper(cpu)
            end
            s = string.format(
                "%s%s: <span color='%s'>%s%%</span>",
                s,
                tmp,
                config.theme.PURPLE_COLOR,
                string.format("%03u", newjiffies - m.jiffies[cpu]))
            m.jiffies[cpu] = newjiffies
        end
    end
    m.widget:set_markup(s)
end

m.widget:set_font(config.theme.WIBOX_TOP_FONT)
m.update()
m.t:connect_signal("timeout", m.update)
m.t:start()

return m
