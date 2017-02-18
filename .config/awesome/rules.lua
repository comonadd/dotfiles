-- File: rules.lua
-- Creation date: 2017-02-08
-- Creator: Dmitry Guzeev <dmitry.guzeev@yahoo.com>
-- Description:
-- Rules for the Awful library

local awful = require("awful")
awful.rules = require("awful.rules")
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
