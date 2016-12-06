local m = {}

local wibox = require("wibox")
local awful = require("awful")
local config = require("config")
local utils = require("utils")

m.widget = wibox.widget.textbox()
m.show = function ()
    awful.prompt.run({prompt = ">>> ", font = config.theme.WIBOX_BOT_FONT},
                    m.widget,
                    function (cmd)
                        if cmd:sub(0, 1) == "!" then
                            local cmd_name = utils.trim(cmd:sub(0, cmd:find(" ")))
                            local cmd_arg = ""

                            if cmd:len() > cmd_name:len() then
                                cmd_arg = utils.trim(cmd:sub(cmd:find(" "), cmd:len())):gsub("%s+", "%%20")
                            end

                            if cmd_name == "!o" then
                                utils.run_cmd(config.BROWSER .. " " .. cmd_arg)
                            elseif cmd_name == "!g" then
                                utils.run_cmd(config.BROWSER .. " " .. config.GOOGLE_SEARCH_URL .. cmd_arg)
                            elseif cmd_name == "!w" then
                                utils.run_cmd(config.BROWSER .. " " .. config.WIKIPEDIA_WIKI_URL .. cmd_arg)
                            end
                        else
                            if utils.contains(cmd, config.ALIASES_NAMES) then
                                utils.run_cmd(config.ALIASES[cmd], function () end)
                            else
                                utils.run_cmd(cmd, function () end)
                            end
                        end
                    end,
                    awful.completion.shell)
end

return m;
