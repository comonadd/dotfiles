-- File: signals.lua
-- Creation date: 2017-02-08
-- Creator: Dmitry Guzeev <dmitry.guzeev@yahoo.com>
-- Description:
-- Signals

client.connect_signal(
   "manage",
   function (c, startup)
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
