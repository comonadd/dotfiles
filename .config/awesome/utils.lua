-- File: utils.lua
-- Creation date: 2017-02-06
-- Creator: Dmitry Guzeev <dmitry.guzeev@yahoo.com>
-- Description:
-- Utility

local m = {}

local io = {
   open = io.open,
   popen = io.popen
}

-- Constants
SPACE_CHAR = " "

-- Variables
local background_timers = {}

-- Functions
-- Helpers
m.rfind = function (s, c)
   local i, j
   local k = 0
   repeat
      i = j
      j, k = s:find(c, k + 1, true)
   until j == nil
   return i
end

m.trim = function (s)
   return s:match'^()%s*$' and '' or s:match'^%s*(.*%S)'
end

m.split_by_newline = function (s)
   local t = {}
   local function helper(line)
      t[#t+1] = line
      return ""
   end
   helper((s:gsub("(.-)\r?\n", helper)))
   return t
end

m.split_by_space = function (s)
   local words = {}
   for word in s:gmatch("%w+") do
      words[#words+1] = word
   end
   return words
end

m.contains = function (v, t)
   for _, v_ in pairs(t) do
      if v_ == v then
         return true
      end
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

m.get_uname = function ()
   local h = io.popen("uname -s")
   local r = h:read("*a"):gsub("\n", "") .. SPACE_CHAR
   h:close()

   h = io.popen("uname -n")
   r = r .. h:read("*a"):gsub("\n", "") .. SPACE_CHAR
   h:close()

   h = io.popen("uname -r")
   r = r .. h:read("*a"):gsub("\n", "") .. SPACE_CHAR
   h:close()

   return r
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
      t = timer{timeout = 1}
   }

   background_timers[cmd].t:connect_signal(
      "timeout",
      function ()
         local cmdf = io.popen("pgrep -f '" .. cmd .. "'")
         local s = cmdf:read("*all")
         cmdf:close()
         if (s == "") then
            background_timers[cmd].t:stop()
            local lf = io.open(background_timers[cmd].file)
            funtocall(lf:read("*all"))
            lf:close()
            io.popen("rm " .. background_timers[cmd].file)
         end
                                          end)
   background_timers[cmd].t:start()
end

return m
