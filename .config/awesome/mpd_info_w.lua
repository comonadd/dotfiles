local m = {}

local wibox = require("wibox")
local config = require("config")
local utils = require("utils")

m.widget = wibox.widget.textbox()
m.t = timer({timeout = config.MPD_INFO_W_TIMEOUT})
m.sock = socket.tcp()
m.status = {}
m.curr_song = {}
m.init = function ()
    if not m.sock:connect("127.0.0.1", 6622) and not m.sock:receive() then
        m.widget:set_markup(
            string.format(
                "<span color='%s'>System</span>  ―  <span color='%s'>Not connected</span> # <span color='%s'>00:00/00:00</span>",
                config.theme.PURPLE_COLOR,
                config.theme.PURPLE_COLOR,
                config.theme.PURPLE_COLOR))
        return nil
    end
end

m.command = function (args)
    if not m.sock:send(args.cmd .. "\n") then return nil end
    local resp = ""
    local tmp = ""

    while true do
        tmp = m.sock:receive()
        resp = resp .. tmp .. "\n"
        if tmp == "OK" then break end
    end

    return resp
end

m.get_curr_song = function ()
    local resp = m.command{cmd = "currentsong"}
    if resp == nil then return nil end
    local current_song = {}

    for k, v in pairs(utils.split_by_newline(resp)) do
        if v:find(":") then
            current_song[v:sub(0, v:find(":") - 1)] = utils.trim(v:sub(v:find(":") + 1, v:len()))
        end
    end

    return current_song
end

m.get_status = function ()
    local resp = m.command{cmd = "status"}
    if resp == nil then return nil end
    local status = {}

    for k, v in pairs(utils.split_by_newline(resp)) do
        if v:find(":") then
            status[v:sub(0, v:find(":") - 1)] = utils.trim(v:sub(v:find(":") + 1, v:len()))
        end
    end

    return status
end

m.pause = function ()
    if m.status["state"] == "pause" then
        m.command{cmd = "play"}
    else
        m.command{cmd = "pause"}
    end
end

m.update = function ()
    local time = {}
    local artist
    local title

    m.status = m.get_status()
    m.curr_song = m.get_curr_song()

    if not m.status or not m.curr_song then
        m.init()
        return nil
    elseif m.status["state"] == "stop" then
        m.widget:set_markup(
            string.format(
                "<span color='%s'>System</span>  ―  <span color='%s'>Not playing</span> # <span color='%s'>00:00/00:00</span>",
                config.theme.PURPLE_COLOR,
                config.theme.PURPLE_COLOR,
                config.theme.PURPLE_COLOR))
    else
        artist = m.curr_song["Artist"] or "Unknown"
        title = m.curr_song["Title"] or "Unknown"
        time.gone = {}
        time.total = {}
        time.gone.seconds = tonumber(
            m.status["time"]:sub(
                0,
                m.status["time"]:find(":") - 1))
        time.gone.minutes = math.floor(time.gone.seconds / 60)
        time.gone.hours = math.floor(time.gone.minutes / 60)
        time.gone.seconds = time.gone.seconds - time.gone.minutes * 60
        time.gone.minutes = time.gone.minutes - time.gone.hours * 60
        time.total.seconds = tonumber(
            m.status["time"]:sub(
                m.status["time"]:find(":") + 1))
        time.total.minutes = math.floor(time.total.seconds / 60)
        time.total.hours = math.floor(time.total.minutes / 60)
        time.total.seconds = time.total.seconds - time.total.minutes * 60
        time.total.minutes = time.total.minutes - time.total.hours * 60

        if time.total.hours > 0 then
            m.widget:set_markup(
                string.format(
                    "<span color='%s'>%s</span>  ―  <span color='%s'>%s</span> # <span color='%s'>%.2u:%.2u:%.2u/%.2u:%.2u:%.2u</span>",
                    config.theme.PURPLE_COLOR,
                    artist,
                    config.theme.PURPLE_COLOR,
                    title,
                    config.theme.PURPLE_COLOR,
                    time.gone.hours,
                    time.gone.minutes,
                    time.gone.seconds,
                    time.total.hours,
                    time.total.minutes,
                    time.total.seconds))
        else
            m.widget:set_markup(
                string.format(
                    "<span color='%s'>%s</span>  ‒  <span color='%s'>%s</span> # <span color='%s'>%.2u:%.2u/%.2u:%.2u</span>",
                    config.theme.PURPLE_COLOR,
                    artist,
                    config.theme.PURPLE_COLOR,
                    title,
                    config.theme.PURPLE_COLOR,
                    time.gone.minutes,
                    time.gone.seconds,
                    time.total.minutes,
                    time.total.seconds))
        end
    end
end

m.widget:set_font(config.theme.WIBOX_TOP_FONT)
m.init()
m.update()
m.t:connect_signal("timeout", m.update)
m.t:start()

return m
