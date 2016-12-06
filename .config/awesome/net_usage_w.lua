local m = {}

local wibox = require("wibox")
local config = require("config")
local utils = require("utils")

m.widget = wibox.widget.textbox()
m.t = timer({timeout = config.NET_USAGE_W_TIMEOUT})
m.total_packets_received = 0
m.total_packets_sent = 0
m.update = function ()
    local tmp = {}
    local total_packets_received_diff = 0
    local total_packets_sent_diff = 0

    for line in io.lines("/proc/net/dev") do
        if line:find(config.NETWORK_INTERFACE) then
            tmp = utils.split_by_space(line)
            if m.total_packets_received ~= 0 and m.total_packets_sent ~= 0 then

                total_packets_received_diff = tmp[2] - m.total_packets_received
                total_packets_sent_diff = tmp[10] - m.total_packets_sent
            end
            m.total_packets_received = tmp[2]
            m.total_packets_sent = tmp[10]
        end
    end

    m.widget:set_markup(
        string.format(
            "Network: <span color='%s'>%u KB/s</span> # <span color='%s'>%u KB/s</span>",
            config.theme.PURPLE_COLOR,
            total_packets_received_diff / 1024,
            config.theme.PURPLE_COLOR,
            total_packets_sent_diff / 1024))
end

m.widget:set_font(config.theme.WIBOX_TOP_FONT)
m.update()
m.t:connect_signal("timeout", m.update)
m.t:start()

return m
