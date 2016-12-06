local m = {}

local wibox = require("wibox")
local config = require("config")


m.widget = wibox.widget.textbox()
m.t = timer({timeout = config.DISK_USAGE_W_TIMEOUT})
m.space_total = 0
m.space_busy = 0
m.color = "#FFFFFF"
m.init = function ()
    local index = 0
    local numbers = {}
    local handle = io.popen("df --total")
    local result = handle:read("*a")

    result = result:sub(
        result:find("total"),
        result:sub(result:find("total")):find("\n"))
    for word in result:gmatch("%w+") do
        numbers[index] = word
        index = index + 1
    end

    m.space_total = numbers[1] / 1024 / 1024
    m.space_busy = numbers[2] / 1024 / 1024
    handle:close()
end

m.update = function ()
    local index = 0
    local numbers = {}
    local handle = io.popen("df --total")
    local result = handle:read("*a")

    result = result:sub(
        result:find("total"),
        result:sub(result:find("total")):find("\n"))
    for word in result:gmatch("%w+") do
        numbers[index] = word
        index = index + 1
    end

    m.space_busy = numbers[2] / 1024 / 1024

    if m.space_busy <= (m.space_total - (m.space_total / 2)) then
        m.color = config.theme.GREEN_COLOR
    elseif m.space_busy <= (m.space_total - (m.space_total / 4)) then
        m.color = config.theme.YELLOW_COLOR
    else
        m.color = config.theme.RED_COLOR
    end

    m.widget:set_markup(
        string.format(
            "Disk: <span color='%s'>%.1fGB/%.1fGB</span>",
            m.color,
            m.space_busy,
            m.space_total))
    handle:close()
end

m.widget:set_font(config.theme.WIBOX_BOT_FONT)
m.init()
m.update()
m.t:connect_signal("timeout", m.update)
m.t:start()

return m
