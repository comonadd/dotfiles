package.path = package.path .. ";/usr/lib64/lua/luarocks/share/lua/5.1/?.lua;/usr/share/awesome/lib/?.lua"
package.cpath = package.cpath .. ";/usr/lib64/lua/luarocks/lib/lua/5.1/?.so"

local socket = require("socket")
local naughty = require("naughty")
local awful = require("awful")
local wibox = require("wibox")
local utils = require("utils")
local config = require("config")
awful.rules = require("awful.rules")
require("awful.autofocus")
local sound_manager = require("sound_manager")
local separator_w = require("separator_w")
local portage_info_w = require("portage_info_w")
local net_usage_w = require("net_usage_w")
local cpu_usage_w = require("cpu_usage_w")
local mpd_info_w = require("mpd_info_w")
local cli_w = require("cli_w")
local datetime_w = require("datetime_w")
local uptime_w = require("uptime_w")
local kbl_w = require("kbl_w")
local mem_usage_w = require("mem_usage_w")
local temp_w = require("temp_w")
local usb_usage_w = require("usb_usage_w")
local disk_usage_w = require("disk_usage_w")
local kernel_info_w = require("kernel_info_w")
local username_w = require("username_w")

local os = {
    execute = os.execute,
    date = os.date
}
local math = {
    floor = math.floor,
    ceil = math.ceil
}
local io = {
    open = io.open,
    popen = io.popen,
    lines = io.lines
}
local string = {
    format = string.format,
    match = string.match,
    upper = string.upper
}

-- Variables
local wibox_top = {}  -- Top panel
local wibox_bot = {}  -- Bottom panel
local tags = {}       -- Tags list
local tag_list = {}   -- Tag list
-- }

-- Tag list
tags = awful.tag({" ➊ ", " ➋ ", " ➌ ", " ➍ ", " ➎ ", " ➏ ", " ➐ ", " ➑ "}, 1, config.LAYOUTS[1])
tag_list.buttons = awful.util.table.join(
    awful.button(
        {},
        1,
        awful.tag.viewonly))
tag_list.base_widget = wibox.layout.fixed.horizontal()
tag_list = awful.widget.taglist(
    1,
    function () return tags end,
    tag_list.buttons,
    {font = config.theme.WIBOX_TOP_FONT})
-- }

-- Widgets boxes initializing
-- Top
wibox_top = awful.wibox({
    position = "top",
    screen = 1,
    height = config.theme.WIBOX_TOP_H,
    bg = config.theme.WIBOX_TOP_BG,
    fg = config.theme.WIBOX_TOP_FG
})
wibox_tl_l = wibox.layout.fixed.horizontal()
wibox_tr_l = wibox.layout.fixed.horizontal()
wibox_top_l = wibox.layout.align.horizontal()

wibox_tl_l:add(tag_list)
wibox_tl_l:add(separator_w)
wibox_tl_l:add(cli_w.widget)

wibox_tr_l:add(separator_w)
wibox_tr_l:add(portage_info_w.widget)
wibox_tr_l:add(separator_w)
wibox_tr_l:add(net_usage_w.widget)
wibox_tr_l:add(separator_w)
wibox_tr_l:add(cpu_usage_w.widget)
wibox_tr_l:add(separator_w)
wibox_tr_l:add(kbl_w.widget)
wibox_tr_l:add(separator_w)
wibox_tr_l:add(datetime_w.date_widget)
wibox_tr_l:add(separator_w)
wibox_tr_l:add(datetime_w.time_widget)
wibox_tr_l:add(wibox.widget.textbox("  "))

wibox_top_l:set_left(wibox_tl_l)
wibox_top_l:set_right(wibox_tr_l)
wibox_top:set_widget(wibox_top_l)
-- }}

-- Bottom
wibox_bot = awful.wibox({
    position = "bottom",
    screen = 1,
    height = config.theme.WIBOX_BOT_H,
    bg = config.theme.WIBOX_BOT_BG,
    fg = config.theme.WIBOX_BOT_FG
})
wibox_bot_l = wibox.layout.align.horizontal()
wibox_bl_l = wibox.layout.fixed.horizontal()
wibox_bm_l = wibox.layout.fixed.horizontal()
wibox_br_l = wibox.layout.fixed.horizontal()

wibox_bl_l:add(wibox.widget.textbox("  "))
wibox_bl_l:add(mem_usage_w.widget)
wibox_bl_l:add(separator_w)
wibox_bl_l:add(temp_w.widget)
wibox_bl_l:add(separator_w)
wibox_bl_l:add(disk_usage_w.widget)
wibox_bl_l:add(separator_w)
wibox_bl_l:add(usb_usage_w.widget)
wibox_bl_l:add(separator_w)

wibox_bm_l:add(mpd_info_w.widget)

wibox_br_l:add(separator_w)
wibox_br_l:add(username_w)
wibox_br_l:add(separator_w)
wibox_br_l:add(kernel_info_w)
wibox_br_l:add(separator_w)
wibox_br_l:add(uptime_w.widget)
wibox_br_l:add(wibox.widget.textbox("  "))
wibox_bot_l:set_left(wibox_bl_l)
wibox_bot_l:set_middle(wibox_bm_l)
wibox_bot_l:set_right(wibox_br_l)
wibox_bot:set_widget(wibox_bot_l)
-- }}
-- }

-- Key bindings
globalkeys = awful.util.table.join(
    awful.key({"Mod4", "Control"}, "p", mpd_info_w.pause),
    awful.key({"Mod4", "Control"}, "m", sound_manager.mute),
    awful.key({"Mod4", "Control"}, "=",
        function ()
            sound_manager.set_sound_level_relative(config.SOUND_LEVEL_STEP)
        end),
    awful.key({"Mod4", "Control"}, "-",
        function ()
            sound_manager.set_sound_level_relative(-config.SOUND_LEVEL_STEP)
        end,
    awful.key({"Mod4"}, "p", cli_w.show),
    awful.key({"Mod4"}, "r", awesome.restart),
    awful.key({"Mod4"}, "k", awesome.quit),
    awful.key({"Mod4"}, "a", function () awful.layout.inc(config.LAYOUTS, -1) end),
    awful.key({"Mod4"}, "d", function () awful.layout.inc(config.LAYOUTS, 1) end),
    awful.key({"Mod4"}, "j", function () utils.run_cmd(config.SCREENSHOT_TOOL) end),
    awful.key({"Mod4"}, "Left", function ()
                                    awful.tag.viewprev()
                                end),
    awful.key({"Mod4"}, "Right", function ()
                                    awful.tag.viewnext()
                                 end),
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
    awful.key({"Mod4"}, "t", function () awful.util.spawn(config.TERMINAL) end),
    awful.key({"Mod4"}, "=", function () awful.tag.incmwfact(-0.05)    end),
    awful.key({"Mod4"}, "-", function () awful.tag.incmwfact(0.05)    end),
    awful.key({"Mod1"}, "Shift_L", kbl_w.switch),
    awful.key({"Shift"}, "Alt_L", kbl_w.switch)
)

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
                      local screen = mouse.screen
                      local tag = awful.tag.gettags(screen)[i]
                      if client.focus and awful.tag.gettags(client.focus.screen)[i] then
                          awful.client.movetotag(tag)
                      end
                  end))
end
root.keys(globalkeys)
-- }

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
    }
}
-- }

-- Signals
client.connect_signal("manage", function (c, startup)
    c.size_hints_honor = false
    if not startup then
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    elseif not c.size_hints.user_position and not c.size_hints.program_position then
        awful.placement.no_offscreen(c)
    end
end)
-- }
