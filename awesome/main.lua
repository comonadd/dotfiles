local socket = require("socket")     -- Socket
local awful = require("awful")       -- Main stuff
local gears = require("gears")       -- Wallpapers, etc
local wibox = require("wibox")       -- Panels stuff
local theme = require("beautiful")   -- Theme
local naughty = require("naughty")   -- Notifications stuff
local utils = require("utils")       -- Utils
awful.rules = require("awful.rules") -- Rules
require("awful.autofocus")           -- Autofocus

-- Initializing environment
local os = {execute = os.execute, date = os.date}
local math = {floor = math.floor, ceil = math.ceil}
local io = {open = io.open, popen = io.popen, lines = io.lines}
local string = {format = string.format, match = string.match, upper = string.upper}
--}}}

-- Defining variables
local wibox_top = {}  -- Top panel
local wibox_bot = {}  -- Bottom panel
local tags = {}       -- Tags list
local tag_list = {}   -- Tag list
local separator       -- Separator

-- Widgets
local cli_w = {}        -- Command line widget
local uptime_w = {}     -- Uptime widget
local temp_w = {}       -- Temperature widget
local datetime_w = {}   -- Date and time widget
local mpd_info_w = {}   -- MPD info widget
local kb_l_w = {}       -- Keyboard layout widget
local mem_usage_w = {}  -- Memory info widget
local cpu_usage_w = {}  -- CPU usage widget
local net_usage_w = {}  -- Network usage widget
local usb_usage_w = {}  -- USB usage widget
local disk_usage_w = {} -- Disk usage widget
local sys_info_w        -- System info widget
local username_w        -- Username widget
--}}}

-- Initializing some stuff
-- Something
theme.init(utils.THEME_PATH)
gears.wallpaper.maximized(theme.wallpaper, 1, true)

-- Separator
separator = wibox.widget.textbox(string.format("<span color='%s'> | </span>", theme.SEPARATOR_COLOR))
separator:set_font(theme.SEPARATOR_FONT)
--}}}

-- Network usage widget
net_usage_w.widget = wibox.widget.textbox()
net_usage_w.t = timer({timeout = utils.NET_USAGE_W_TIMEOUT})
net_usage_w.total_packets_received = 0
net_usage_w.total_packets_sent = 0
net_usage_w.update = function ()
    local tmp = {}
    local total_packets_received_diff = 0
    local total_packets_sent_diff = 0

    for line in io.lines("/proc/net/dev") do
        if line:find(utils.NETWORK_CARD_NAME) then
            tmp = utils.split_by_space(line)
            if net_usage_w.total_packets_received ~= 0 and net_usage_w.total_packets_sent ~= 0 then

                total_packets_received_diff = tmp[2] - net_usage_w.total_packets_received
                total_packets_sent_diff = tmp[10] - net_usage_w.total_packets_sent
            end
            net_usage_w.total_packets_received = tmp[2]
            net_usage_w.total_packets_sent = tmp[10]
        end
    end
    net_usage_w.widget:set_markup(string.format("Network: <span color='%s'>%u KB/s</span> # <span color='%s'>%u KB/s</span>",
        theme.PURPLE_COLOR, total_packets_received_diff / 1024,
        theme.PURPLE_COLOR, total_packets_sent_diff / 1024))
end
net_usage_w.update()
net_usage_w.t:connect_signal("timeout", net_usage_w.update)
net_usage_w.t:start()
--}}}

-- CPU usage widget
cpu_usage_w.widget = wibox.widget.textbox()
cpu_usage_w.t = timer({timeout = utils.CPU_USAGE_W_TIMEOUT})
cpu_usage_w.jiffies = {}
cpu_usage_w.update = function ()
    local s = ""
    local tmp = ""

    for line in io.lines("/proc/stat") do
        local cpu, newjiffies = string.match(line, "(cpu%d*)\ +(%d+)")
        if cpu and newjiffies then
            if not cpu_usage_w.jiffies[cpu] then
                cpu_usage_w.jiffies[cpu] = newjiffies
            end
            if s:len() > 0 then
                tmp = string.upper("  " .. cpu)
            else
                tmp = string.upper(cpu)
            end
            s = string.format("%s%s: <span color='%s'>%s%%</span>", s, tmp, theme.PURPLE_COLOR, string.format("%03u", newjiffies - cpu_usage_w.jiffies[cpu]))
            cpu_usage_w.jiffies[cpu] = newjiffies
        end
    end
    cpu_usage_w.widget:set_markup(s)
end
cpu_usage_w.update()
cpu_usage_w.t:connect_signal("timeout", cpu_usage_w.update)
cpu_usage_w.t:start()
--}}}

-- MPD info widget
mpd_info_w.widget = wibox.widget.textbox()
mpd_info_w.t = timer({timeout = utils.MPD_INFO_W_TIMEOUT})
mpd_info_w.sock = socket.tcp()
mpd_info_w.status = {}
mpd_info_w.curr_song = {}
mpd_info_w.init = function ()
    if not mpd_info_w.sock:connect("127.0.0.1", 6622) and not mpd_info_w.sock:receive() then
        mpd_info_w.widget:set_markup(string.format("<span color='%s'>System</span>  ―  <span color='%s'>Not connected</span> # <span color='%s'>00:00/00:00</span>",
            theme.PURPLE_COLOR,
            theme.PURPLE_COLOR,
            theme.PURPLE_COLOR
        ))
        return nil
    end
end
mpd_info_w.command = function (args)
    if not mpd_info_w.sock:send(args.cmd .. "\n") then return nil end
    local resp = ""
    local tmp = ""

    while true do
        tmp = mpd_info_w.sock:receive()
        resp = resp .. tmp .. "\n"
        if tmp == "OK" then break end
    end

    return resp
end
mpd_info_w.get_curr_song = function ()
    local resp = mpd_info_w.command{cmd = "currentsong"}
    if resp == nil then return nil end
    local current_song = {}

    for k, v in pairs(utils.split_by_newline(resp)) do
        if v:find(":") then
            current_song[v:sub(0, v:find(":") - 1)] = utils.trim(v:sub(v:find(":") + 1, v:len()))
        end
    end

    return current_song
end
mpd_info_w.get_status = function ()
    local resp = mpd_info_w.command{cmd = "status"}
    if resp == nil then return nil end
    local status = {}

    for k, v in pairs(utils.split_by_newline(resp)) do
        if v:find(":") then
            status[v:sub(0, v:find(":") - 1)] = utils.trim(v:sub(v:find(":") + 1, v:len()))
        end
    end

    return status
end
mpd_info_w.pause = function ()
    if mpd_info_w.status["state"] == "pause" then
        mpd_info_w.command{cmd = "play"}
    else
        mpd_info_w.command{cmd = "pause"}
    end
end
mpd_info_w.update = function ()
    local time = {}

    mpd_info_w.status = mpd_info_w.get_status()
    mpd_info_w.curr_song = mpd_info_w.get_curr_song()

    if not mpd_info_w.status or not mpd_info_w.curr_song then
        mpd_info_w.init()
        return nil
    elseif mpd_info_w.status["state"] == "stop" then
        mpd_info_w.widget:set_markup(string.format("<span color='%s'>System</span>  ―  <span color='%s'>Not playing</span> # <span color='%s'>00:00/00:00</span>",
            theme.PURPLE_COLOR,
            theme.PURPLE_COLOR,
            theme.PURPLE_COLOR
        ))
    else
        time.gone = {}
        time.total = {}
        time.gone.seconds = tonumber(mpd_info_w.status["time"]:sub(0, mpd_info_w.status["time"]:find(":") - 1))
        time.gone.minutes = math.floor(time.gone.seconds / 60)
        time.gone.hours = math.floor(time.gone.minutes / 60)
        time.gone.seconds = time.gone.seconds - time.gone.minutes * 60
        time.gone.minutes = time.gone.minutes - time.gone.hours * 60
        time.total.seconds = tonumber(mpd_info_w.status["time"]:sub(mpd_info_w.status["time"]:find(":") + 1))
        time.total.minutes = math.floor(time.total.seconds / 60)
        time.total.hours = math.floor(time.total.minutes / 60)
        time.total.seconds = time.total.seconds - time.total.minutes * 60
        time.total.minutes = time.total.minutes - time.total.hours * 60
        if time.total.hours > 0 then
            mpd_info_w.widget:set_markup(string.format("<span color='%s'>%s</span>  ―  <span color='%s'>%s</span> # <span color='%s'>%.2u:%.2u:%.2u/%.2u:%.2u:%.2u</span>",
                theme.PURPLE_COLOR,
                mpd_info_w.curr_song["Artist"],
                theme.PURPLE_COLOR,
                mpd_info_w.curr_song["Title"],
                theme.PURPLE_COLOR,
                time.gone.hours,
                time.gone.minutes,
                time.gone.seconds,
                time.total.hours,
                time.total.minutes,
                time.total.seconds
            ))
        else
            mpd_info_w.widget:set_markup(string.format("<span color='%s'>%s</span>  ‒  <span color='%s'>%s</span> # <span color='%s'>%.2u:%.2u/%.2u:%.2u</span>",
                theme.PURPLE_COLOR,
                mpd_info_w.curr_song["Artist"],
                theme.PURPLE_COLOR,
                mpd_info_w.curr_song["Title"],
                theme.PURPLE_COLOR,
                time.gone.minutes,
                time.gone.seconds,
                time.total.minutes,
                time.total.seconds
            ))
        end
    end
end
mpd_info_w.widget:set_font(theme.WIBOX_TOP_FONT)
mpd_info_w.init()
mpd_info_w.update()
mpd_info_w.t:connect_signal("timeout", mpd_info_w.update)
mpd_info_w.t:start()
--}}}

-- Command line
cli_w.widget = wibox.widget.textbox()
cli_w.show = function ()
    awful.prompt.run({prompt = ">>> ", font = theme.WIBOX_BOTTOM_FONT},
                    cli_w.widget,
                    function (cmd)
                        if cmd:sub(0, 1) == "!" then
                            local cmd_name = utils.trim(cmd:sub(0, cmd:find(" ")))
                            local cmd_arg = ""

                            if cmd:len() > cmd_name:len() then
                                cmd_arg = utils.trim(cmd:sub(cmd:find(" "), cmd:len())):gsub("%s+", "%%20")
                            end

                            if cmd_name == "!o" then
                                utils.run_cmd(utils.BROWSER .. " " .. cmd_arg)
                            elseif cmd_name == "!oi" then
                                utils.run_cmd(utils.BROWSER .. " --incognito " .. cmd_arg)
                            elseif cmd_name == "!g" then
                                utils.run_cmd(utils.BROWSER .. " " .. utils.GOOGLE_SEARCH_URL .. cmd_arg)
                            elseif cmd_name == "!gi" then
                                utils.run_cmd(utils.BROWSER .. " --incognito " .. utils.GOOGLE_SEARCH_URL .. cmd_arg)
                            elseif cmd_name == "!w" then
                                utils.run_cmd(utils.BROWSER .. " " .. utils.WIKIPEDIA_WIKI_URL .. cmd_arg)
                            elseif cmd_name == "!vk" then
                                utils.run_cmd(utils.BROWSER .. " " .. utils.VK_URL)
                            end
                        else
                            if utils.contains(cmd, utils.ALIASES_NAMES) then
                                utils.run_cmd(utils.ALIASES[cmd], function () end)
                            else
                                utils.run_cmd(cmd, function () end)
                            end
                        end
                    end,
                    awful.completion.shell)
end
--}}}

-- Datetime widget
datetime_w.time_widget = wibox.widget.textbox()
datetime_w.date_widget = wibox.widget.textbox()
datetime_w.t = timer({timeout = 60})
datetime_w.update = function ()
    datetime_w.time_widget:set_text(os.date("%H:%M"))
    datetime_w.date_widget:set_text(os.date("%d/%m"))
end
datetime_w.time_widget:set_font(theme.WIBOX_TOP_FONT)
datetime_w.date_widget:set_font(theme.WIBOX_TOP_FONT)
datetime_w.update()
datetime_w.t:connect_signal("timeout", datetime_w.update)
datetime_w.t:start()
--}}}

-- Uptime widget
uptime_w.widget = wibox.widget.textbox()
uptime_w.t = timer({timeout = 1})
uptime_w.uptime = {}
uptime_w.update = function ()
    local file = io.open("/proc/uptime", "r")

    uptime_w.uptime.seconds = file:read()
    uptime_w.uptime.seconds = tonumber(uptime_w.uptime.seconds:sub(0, uptime_w.uptime.seconds:find(" ")))
    uptime_w.uptime.hours = math.floor(uptime_w.uptime.seconds / 3600)
    uptime_w.uptime.minutes = math.floor((uptime_w.uptime.seconds - uptime_w.uptime.hours * 3600) / 60)
    uptime_w.widget:set_markup(string.format("Uptime: <span color='%s'>%.2i:%.2i</span>", theme.PURPLE_COLOR, uptime_w.uptime.hours, uptime_w.uptime.minutes))
    file:close()
end
uptime_w.widget:set_font(theme.WIBOX_BOTTOM_FONT)
uptime_w.update()
uptime_w.t:connect_signal("timeout", uptime_w.update)
uptime_w.t:start()
--}}}

-- Tag list
tags = awful.tag({" ➊ ", " ➋ ", " ➌ ", " ➍ ", " ➎ ", " ➏ ", " ➐ ", " ➑ "}, 1, utils.LAYOUTS[1])
tag_list.buttons = awful.util.table.join(awful.button({}, 1, awful.tag.viewonly))
tag_list = awful.widget.taglist(1, function () return tags end, tag_list.buttons)
--}}}

-- Keyboard layout widget
kb_l_w.widget = wibox.widget.textbox()
kb_l_w.current = 1
kb_l_w.layout = {
    {"us", "US"},
    {"ru", "RU"}
}
kb_l_w.switch = function ()
    local t
    kb_l_w.current = kb_l_w.current % #(kb_l_w.layout) + 1
    t = kb_l_w.layout[kb_l_w.current]
    kb_l_w.widget:set_text(t[2])
    os.execute("setxkbmap " .. t[1])
end
kb_l_w.widget:set_font(theme.WIBOX_TOP_FONT)
kb_l_w.widget:set_text(kb_l_w.layout[kb_l_w.current][2])
--}}}

-- Memory widget
mem_usage_w.widget = wibox.widget.textbox()
mem_usage_w.t = timer({timeout = utils.MEM_USAGE_W_TIMEOUT})
mem_usage_w.ram_total = 0
mem_usage_w.swap_total = 0
mem_usage_w.ram_busy = 0
mem_usage_w.swap_busy = 0
mem_usage_w.ram_color = "#FFFFFF"
mem_usage_w.swap_color = "#FFFFFF"
mem_usage_w.init = function()
    for line in io.lines("/proc/meminfo") do
        local count

        if line:find("MemTotal") then
            line = line:sub(0, -3)
            line, count = line:sub(line:find("MemTotal") - #line + 8):gsub("[^%w]", "")
            mem_usage_w.ram_total = math.ceil(tonumber(line) / 1024)
        elseif line:find("SwapTotal") then
            line = line:sub(0, -3)
            line, count = line:sub(line:find("SwapTotal") - #line + 9):gsub("[^%w]", "")
            mem_usage_w.swap_total = math.ceil(tonumber(line) / 1024)
        end
    end
end
mem_usage_w.update = function ()
    for line in io.lines("/proc/meminfo") do
        local count

        if line:find("MemAvailable") then
            line = line:sub(0, -3)
            line, count = line:sub(line:find("MemAvailable") - #line + 12):gsub("%W", '')
            mem_usage_w.ram_busy = math.ceil(tonumber(line) / 1024)
        elseif line:find("SwapFree") then
            line = line:sub(0, -3)
            line, count = line:sub(line:find("SwapFree") - #line + 8):gsub("%W", '')
            mem_usage_w.swap_busy = math.ceil(tonumber(line) / 1024)
        end
    end
    mem_usage_w.ram_busy = mem_usage_w.ram_total - mem_usage_w.ram_busy
    mem_usage_w.swap_busy = mem_usage_w.swap_total - mem_usage_w.swap_busy

    if mem_usage_w.ram_busy <= mem_usage_w.ram_total - mem_usage_w.ram_total / 2 then
        mem_usage_w.ram_color = theme.GREEN_COLOR
    elseif mem_usage_w.ram_busy <= mem_usage_w.ram_total - mem_usage_w.ram_total / 4 then
        mem_usage_w.ram_color = theme.YELLOW_COLOR
    else
        mem_usage_w.ram_color = theme.RED_COLOR
    end

    if mem_usage_w.swap_busy <= mem_usage_w.swap_total - mem_usage_w.swap_total / 2 then
        mem_usage_w.swap_color = theme.GREEN_COLOR
    elseif mem_usage_w.swap_busy <= mem_usage_w.swap_total - mem_usage_w.swap_total / 4 then
        mem_usage_w.swap_color = theme.YELLOW_COLOR
    else
        mem_usage_w.swap_color = theme.RED_COLOR
    end

    mem_usage_w.widget:set_markup(string.format("Mem: <span color='%s'>RAM: %iMB/%iMB</span> # <span color='%s'>SWAP: %uMB/%uMB</span>", mem_usage_w.ram_color, mem_usage_w.ram_busy, mem_usage_w.ram_total, mem_usage_w.swap_color, mem_usage_w.swap_busy, mem_usage_w.swap_total))
end
mem_usage_w.widget:buttons(
    awful.util.table.join(awful.button({}, 1, mem_usage_w.update))
)
mem_usage_w.widget:set_font(theme.WIBOX_BOTTOM_FONT)
mem_usage_w.init()
mem_usage_w.update()
mem_usage_w.t:connect_signal("timeout", mem_usage_w.update)
mem_usage_w.t:start()
--}}}

-- Temperature widget
temp_w.widget = wibox.widget.textbox()
temp_w.t = timer({timeout = utils.TEMP_W_TIMEOUT})
temp_w.sys_temp = 0
temp_w.temp = 0
temp_w.sys_temp_max = 0
temp_w.temp_max = 0
temp_w.sys_color = "#FFFFFF"
temp_w.color = "#FFFFFF"
temp_w.init = function ()
    local file

    file = io.open("/sys/class/hwmon/hwmon1/subsystem/hwmon0/temp2_max")
    temp_w.temp_max = tonumber(file:read()) / 1000
    if temp_w.temp_max <= 0 then
        temp_w.temp_max = utils.TEMP_W_MAX_TEMP
    end
    file:close()

    file = io.open("/sys/class/hwmon/hwmon1/subsystem/hwmon0/temp1_max")
    temp_w.sys_temp_max = tonumber(file:read()) / 1000
    if temp_w.sys_temp_max <= 0 then
        temp_w.sys_temp_max = utils.TEMP_W_MAX_TEMP
    end
    file:close()
end
temp_w.update = function ()
    local file

    file = io.open("/sys/class/hwmon/hwmon1/subsystem/hwmon0/temp2_input")
    temp_w.temp = tonumber(file:read()) / 1000
    file:close()

    file = io.open("/sys/class/hwmon/hwmon1/subsystem/hwmon0/temp1_input")
    temp_w.sys_temp = tonumber(file:read()) / 1000
    file:close()

    if temp_w.sys_temp <= (temp_w.sys_temp_max - (temp_w.sys_temp_max / 2)) then
        temp_w.sys_color = theme.GREEN_COLOR
    elseif temp_w.sys_temp <= (temp_w.sys_temp_max - (temp_w.sys_temp_max / 4)) then
        temp_w.sys_color = theme.YELLOW_COLOR
    else
        temp_w.sys_color = theme.RED_COLOR
    end

    if temp_w.temp <= (temp_w.temp_max - (temp_w.temp_max / 2)) then
        temp_w.color = theme.GREEN_COLOR
    elseif temp_w.temp <= (temp_w.temp_max - (temp_w.temp_max / 4)) then
        temp_w.color = theme.YELLOW_COLOR
    else
        temp_w.color = theme.RED_COLOR
    end

    temp_w.widget:set_markup(string.format("Temp: <span color='%s'>SYSTIN: %.1f°C</span> # <span color='%s'>CPUTIN: %.1f°C</span>", temp_w.sys_color, temp_w.sys_temp, temp_w.color, temp_w.temp))
end
temp_w.widget:set_font(theme.WIBOX_BOTTOM_FONT)
temp_w.init()
temp_w.update()
temp_w.t:connect_signal("timeout", temp_w.update)
temp_w.t:start()
--}}}

-- Disk widget
disk_usage_w.widget = wibox.widget.textbox()
disk_usage_w.t = timer({timeout = utils.DISK_USAGE_W_TIMEOUT})
disk_usage_w.space_total = 0
disk_usage_w.space_busy = 0
disk_usage_w.color = "#FFFFFF"
disk_usage_w.init = function ()
    local index = 0
    local numbers = {}
    local handle = io.popen("df --total")
    local result = handle:read("*a")

    result = result:sub(result:find("total"), result:sub(result:find("total")):find("\n"))
    for word in result:gmatch("%w+") do
        numbers[index] = word
        index = index + 1
    end

    disk_usage_w.space_total = numbers[1] / 1024 / 1024
    disk_usage_w.space_busy = numbers[2] / 1024 / 1024
    handle:close()
end
disk_usage_w.update = function ()
    local index = 0
    local numbers = {}
    local handle = io.popen("df --total")
    local result = handle:read("*a")

    result = result:sub(result:find("total"), result:sub(result:find("total")):find("\n"))
    for word in result:gmatch("%w+") do
        numbers[index] = word
        index = index + 1
    end

    disk_usage_w.space_busy = numbers[2] / 1024 / 1024

    if disk_usage_w.space_busy <= (disk_usage_w.space_total - (disk_usage_w.space_total / 2)) then
        disk_usage_w.color = theme.GREEN_COLOR
    elseif disk_usage_w.space_busy <= (disk_usage_w.space_total - (disk_usage_w.space_total / 4)) then
        disk_usage_w.color = theme.YELLOW_COLOR
    else
        disk_usage_w.color = theme.RED_COLOR
    end

    disk_usage_w.widget:set_markup(string.format("Disk: <span color='%s'>%.1fGB/%.1fGB</span>", disk_usage_w.color, disk_usage_w.space_busy, disk_usage_w.space_total))
    handle:close()
end
disk_usage_w.widget:set_font(theme.WIBOX_BOTTOM_FONT)
disk_usage_w.init()
disk_usage_w.update()
disk_usage_w.t:connect_signal("timeout", disk_usage_w.update)
disk_usage_w.t:start()
--}}}

-- USB widget
usb_usage_w.widget = wibox.widget.textbox()
usb_usage_w.t = timer({timeout = utils.USB_USAGE_W_TIMEOUT})
usb_usage_w.count = 0
usb_usage_w.update = function ()
    local handle = io.popen("lsusb")
    local result = handle:read("*a")

    for index, line in pairs(utils.split_by_newline(result)) do
        if (#line > 0) and (not line:find("Linux Foundation")) then
            usb_usage_w.count = usb_usage_w.count + 1
        end
    end
    usb_usage_w.widget:set_markup(string.format("USB: <span color='%s'>%u</span>", theme.PURPLE_COLOR, usb_usage_w.count))
    usb_usage_w.count = 0
    handle:close()
end
usb_usage_w.widget:set_font(theme.WIBOX_BOTTOM_FONT)
usb_usage_w.update()
usb_usage_w.t:connect_signal("timeout", usb_usage_w.update)
usb_usage_w.t:start()
--}}}

-- Kernel info widget
sys_info_w = wibox.widget.textbox(string.format("<span color='%s'>%s</span>", theme.PURPLE_COLOR, utils.UNAME))
sys_info_w:set_font(theme.WIBOX_BOTTOM_FONT)
--}}}

-- Username widget
username_w = wibox.widget.textbox(string.format("<span color='%s'>%s</span>", theme.PURPLE_COLOR, utils.USERNAME))
username_w:set_font(theme.WIBOX_BOTTOM_FONT)
--}}}
--}}}

-- Widgets boxes initializing
-- Top
wibox_top = awful.wibox({
    position = "top",
    screen = 1,
    height = utils.WIBOX_TOP_H
})
wibox_tl_l = wibox.layout.fixed.horizontal()
wibox_tr_l = wibox.layout.fixed.horizontal()
wibox_top_l = wibox.layout.align.horizontal()

wibox_tl_l:add(tag_list)
wibox_tl_l:add(separator)
wibox_tl_l:add(cli_w.widget)

wibox_tr_l:add(separator)
wibox_tr_l:add(net_usage_w.widget)
wibox_tr_l:add(separator)
wibox_tr_l:add(cpu_usage_w.widget)
wibox_tr_l:add(separator)
wibox_tr_l:add(kb_l_w.widget)
wibox_tr_l:add(separator)
wibox_tr_l:add(datetime_w.date_widget)
wibox_tr_l:add(separator)
wibox_tr_l:add(datetime_w.time_widget)
wibox_tr_l:add(wibox.widget.textbox("  "))

wibox_top_l:set_left(wibox_tl_l)
wibox_top_l:set_right(wibox_tr_l)
wibox_top:set_widget(wibox_top_l)
--}}}

-- Bottom
wibox_bot = awful.wibox({
    position = "bottom",
    screen = 1,
    height = utils.WIBOX_BOTTOM_H
})
wibox_bot_l = wibox.layout.align.horizontal()
wibox_bl_l = wibox.layout.fixed.horizontal()
wibox_bm_l = wibox.layout.fixed.horizontal()
wibox_br_l = wibox.layout.fixed.horizontal()

wibox_bl_l:add(wibox.widget.textbox("  "))
wibox_bl_l:add(mem_usage_w.widget)
wibox_bl_l:add(separator)
wibox_bl_l:add(temp_w.widget)
wibox_bl_l:add(separator)
wibox_bl_l:add(disk_usage_w.widget)
wibox_bl_l:add(separator)
wibox_bl_l:add(usb_usage_w.widget)
wibox_bl_l:add(separator)

wibox_bm_l:add(mpd_info_w.widget)

wibox_br_l:add(separator)
wibox_br_l:add(username_w)
wibox_br_l:add(separator)
wibox_br_l:add(sys_info_w)
wibox_br_l:add(separator)
wibox_br_l:add(uptime_w.widget)
wibox_br_l:add(wibox.widget.textbox("  "))

wibox_bot_l:set_left(wibox_bl_l)
wibox_bot_l:set_middle(wibox_bm_l)
wibox_bot_l:set_right(wibox_br_l)
wibox_bot:set_widget(wibox_bot_l)
--}}}
--}}}

-- Key bindings
-- Global keys
globalkeys = awful.util.table.join(
    awful.key({"Mod4", "Control"}, "p", mpd_info_w.pause),
    awful.key({"Mod4", "Control"}, "m", function ()
        if utils.get_sys_sound_level() > 0 then
            utils.SOUND_LEVEL = utils.get_sys_sound_level()
            utils.set_sys_sound_level(0)
        else
            if utils.SOUND_LEVEL > 0 then
                utils.set_sys_sound_level(utils.SOUND_LEVEL)
            end
        end
    end),
    awful.key({"Mod4"}, "p", cli_w.show),
    awful.key({"Mod4"}, "r", awesome.restart),
    awful.key({"Mod4"}, "k", awesome.quit),
    awful.key({"Mod4"}, "a", function () awful.layout.inc(utils.LAYOUTS, -1) end),
    awful.key({"Mod4"}, "d", function () awful.layout.inc(utils.LAYOUTS, 1) end),
    awful.key({"Mod4"}, "j", function () utils.run_cmd(utils.SCREENSHOT_TOOL) end),
    awful.key({"Mod4"}, "Left", awful.tag.viewprev),
    awful.key({"Mod4"}, "Right", awful.tag.viewnext),
    awful.key({"Mod4"}, "q",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({"Mod4"}, "e",
        function ()
            awful.client.focus.byidx(1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({"Mod4"}, "t", function () awful.util.spawn(utils.TERMINAL) end),
    awful.key({"Mod4"}, "=", function () awful.tag.incmwfact(-0.05)    end),
    awful.key({"Mod4"}, "-", function () awful.tag.incmwfact(0.05)    end),
    awful.key({"Mod1"}, "Shift_L", kb_l_w.switch),
    awful.key({"Shift"}, "Alt_L", kb_l_w.switch)
)
-- Keys for each window
clientkeys = awful.util.table.join(
    awful.key({"Mod4"}, "c", function (c) c:kill() end),
    awful.key({"Mod4"}, "s", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({"Mod4"}, "f", function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({"Mod4"}, "z", function (c) c.minimized = true end),
    awful.key({"Mod4"}, "x", function (c) c.minimized = false end),
    awful.key({"Mod4"}, "m", function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical = not c.maximized_vertical
    end)
)
clientbuttons = awful.util.table.join(
    awful.button({}, 1, function (c) client.focus = c; c:raise() end)
)
for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({"Mod4"}, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        local tag = awful.tag.gettags(screen)[i]
                        if tag then
                           awful.tag.viewonly(tag)
                        end
                  end),
        awful.key({"Mod4", "Control"}, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      local tag = awful.tag.gettags(screen)[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end),
        awful.key({"Mod4", "Shift"}, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = awful.tag.gettags(client.focus.screen)[i]
                          if tag then
                              awful.client.movetotag(tag)
                          end
                     end
                  end))
end
root.keys(globalkeys)
--}}}

-- Rules
awful.rules.rules = {
    {
        rule = {},
        properties = {
            focus = awful.client.focus.filter,
            raise = true,
            keys = clientkeys,
            buttons = clientbuttons,
            size_hints_honor = false
        }
    },
    {
        rule = {class = "gimp"},
        properties = {floating = true}
    }
}
--}}}

-- Signals
client.connect_signal("manage", function (c, startup)
    if not startup then
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    elseif not c.size_hints.user_position and not c.size_hints.program_position then
        awful.placement.no_offscreen(c)
    end
end)
--}}}
