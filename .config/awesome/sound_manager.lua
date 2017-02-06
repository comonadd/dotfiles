-- File: sound_manager.lua
-- Creation date: 2017-02-06
-- Creator: Dmitry Guzeev <dmitry.guzeev@yahoo.com>
-- Description:
-- The sound manager

local m = {}

local utils = require("utils")

m.get_sound_level = function ()
   local h = io.popen("amixer get 'Master'")
   local data = h:read("*a")
   h:close()
   for _, line in pairs(utils.split_by_newline(data)) do
      if line:sub(0, 6) == "  Mono" then
         line = utils.split_by_space(line)
         return tonumber(line[4])
      end
   end
   return -1
end

m.is_muted = function ()
   local h = io.popen("amixer get 'Master'")
   local data = h:read("*a")
   h:close()
   for _, line in pairs(utils.split_by_newline(data)) do
      if line:sub(0, 6) == "  Mono" then
         line = utils.split_by_space(line)
         return line[7] == "off"
      end
   end
   return false
end

m.set_sound_level = function (v)
   m.CURRENT_SOUND_LEVEL = v
   os.execute("amixer set 'Master' " .. v .. "%")
end

m.set_sound_level_relative = function (v)
   m.set_sound_level(m.CURRENT_SOUND_LEVEL + v)
end

m.mute = function ()
   if m.is_muted() then
      os.execute("amixer set 'Master' on")
   else
      os.execute("amixer set 'Master' off")
   end
end

m.CURRENT_SOUND_LEVEL = m.get_sound_level()

return m
