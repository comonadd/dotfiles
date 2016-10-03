local m = {
    nil, nil, nil, nil,
    nil, nil, nil, nil,
    nil, nil, nil, nil,
    nil, nil, nil, nil,
    nil, nil, nil, nil,
    nil, nil, nil, nil,
    nil, nil, nil, nil,
    nil, nil, nil, nil,
    nil, nil, nil, nil,
    nil, nil, nil, nil,
    nil, nil, nil, nil,
    nil, nil
}
local io = {
    popen = io.popen
}
local os = {
    execute = os.execute
}
local awful = require("awful")
local awful = {
    layout = awful.layout
}

-- Functions
m.get_uname = function ()
    local h = io.popen("uname -s")
    local r = h:read("*a"):gsub("\n", "") .. " "
    h:close()

    h = io.popen("uname -n")
    r = r .. h:read("*a"):gsub("\n", "") .. " "
    h:close()

    h = io.popen("uname -r")
    r = r .. h:read("*a"):gsub("\n", "") .. " "
    h:close()

    return r
end
m.trim = function (s)
    return s:match'^()%s*$' and '' or s:match'^%s*(.*%S)'
end
m.split_by_newline = function (s)
    local t = {}
    local function helper(line) t[#t+1] = line return "" end
    helper((s:gsub("(.-)\r?\n", helper)))
    return t
end
m.split_by_space = function (s)
    local words = {}
    for word in s:gmatch("%w+") do words[#words+1] = word end
    return words
end
m.contains = function (v, t)
    for _, v_ in pairs(t) do
        if v_ == v then return true end
    end
    return false
end
m.keys = function (tab)
    local n = 0
    local keys = {}

    for k, _ in pairs(tab) do
        keys[n] = k
        n = n + 1
    end
    return keys
end
m.run_cmd = function (cmd, funtocall)
    local r = io.popen("mktemp")
    local logfile = r:read("*line")
    local cmdstr
    r:close()

    cmdstr = cmd .. " &> " .. logfile .. " & "
    local cmdf = io.popen(cmdstr)
    cmdf:close()
    background_timers[cmd] = {
        file  = logfile,
        timer = timer{timeout=1}
    }
    background_timers[cmd].t:add_signal("timeout", function()
        local cmdf = io.popen("pgrep -f '" .. cmd .. "'")
        local s = cmdf:read("*all")
        cmdf:close()
        if (s=="") then
            background_timers[cmd].t:stop()
            local lf = io.open(background_timers[cmd].file)
            funtocall(lf:read("*all"))
            lf:close()
            io.popen("rm " .. background_timers[cmd].file)
        end
    end)
    background_timers[cmd].t:start()
end
m.get_sys_sound_level = function ()
    local h = io.popen("amixer get 'Master'")
    local data = h:read("*a")
    h:close()
    for _, line in pairs(m.split_by_newline(data)) do
        if line:sub(0, 6) == "  Mono" then
            line = m.split_by_space(line)
            return tonumber(line[4])
        end
    end
end
m.set_sys_sound_level = function ()
    os.execute("amixer sset 'Master' " .. v .. "%")
end
--}}}

-- Widget boxes settings
-- Top wibox
m.WIBOX_TOP_H = 20

-- Bottom wibox
m.WIBOX_BOTTOM_H = 20
--}}}

-- Widgets settings
-- MPD info widget
m.MPD_INFO_W_TIMEOUT = 1       -- MPD widget update timeout, in seconds

-- Memory usage widget
m.MEM_USAGE_W_TIMEOUT = 8      -- Memory usage widget update timout, in seconds

-- USB usage widget
m.USB_USAGE_W_TIMEOUT = 16     -- USB usage widget update timeout, in seconds

-- CPU usage widget
m.CPU_USAGE_W_TIMEOUT = 1      -- CPU usage widget update timeout, in seconds

-- Network usage widget
m.NET_USAGE_W_TIMEOUT = 1      -- Network usage widget update timeout, in seconds

-- Disk usage widget
m.DISK_USAGE_W_TIMEOUT = 32    -- Disk usage widget update timeout, in seconds

-- Temperature widget
m.TEMP_W_TIMEOUT = 8           -- Temperature widget update timeout, in seconds
m.TEMP_W_MAX_TEMP = 80         -- Temperature widget max
--}}}

-- Constants
m.USERNAME = "wrongway4you"
m.UNAME = m.get_uname()
m.THEME_PATH = "~/.themes/user/theme.lua"
m.EDITOR = "subl"
m.BROWSER = "google-chrome"
m.TERMINAL = "urxvt"
m.SCREENSHOT_TOOL = "gnome-screenshot"
m.VK_URL = "https://vk.com/"
m.GOOGLE_SEARCH_URL = "https://www.google.com.ua/search?q="
m.WIKIPEDIA_WIKI_URL = "https://en.wikipedia.org/wiki/"
m.SOUND_LEVEL = m.get_sys_sound_level()
m.NETWORK_CARD_NAME = "enp2s0"
m.DEFAULT_SOUND_LEVEL = 80
m.LAYOUTS = {
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.max
}
m.ALIASES = {
    term = m.TERMINAL,
    uX = "xrdb ~/.Xresources",
    eA = m.EDITOR .. " ~/.config/awesome/*.lua",
    eT = m.EDITOR .. " " .. m.THEME_PATH
}
m.ALIASES_NAMES = m.keys(m.ALIASES)
--}}}

return m