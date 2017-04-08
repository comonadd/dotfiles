-- XMobar main configuration file
-- Creation date: 2017/03/05
-- Author: wrongway4you

Config {
  font = "-*-Fixed-Bold-R-Normal-*-13-*-*-*-*-*-*-*",
  fgColor = "grey",
  bgColor = "black",
  alpha = 128,
  position = Static {xpos = 0, ypos = 0, width = 1920, height = 24},

  lowerOnStart = True,    -- send to bottom of window stack on start
  hideOnStart = False,   -- start with window unmapped (hidden)
  allDesktops = True,    -- show on all desktops
  -- overrideRedirect = True,    -- set the Override Redirect flag (Xlib)
  -- pickBroadest = False,   -- choose widest display (multi-monitor)
  -- persistent = True,    -- enable/disable hiding (True = disabled)

  border = NoBorder,
  commands = [
    -- CPU
    Run Cpu [
        "-t", "<fc=#ee9a00>Cpu</fc>: <total>%",
        "-L", "3",
        "-H", "50"
    ] 10,

    -- Memory
    Run Memory [
        "-t", "<fc=#ee9a00>Mem</fc>: <usedratio>%"
    ] 10,

    -- Swap
    Run Swap [
        "-t", "<fc=#ee9a00>Swap</fc>: <usedratio>%"
    ] 10,

    -- MPD
    Run MPD [
      "-t", "<fc=#00ff00><artist></fc> -- <fc=#ee9a00><title></fc> [<lapsed>]",
      "-x", "Not playing",
      "--",
      "-P", ">>",
      "-Z", "|",
      "-S", "><"
    ] 10,

    -- Weather
    Run Com "/home/wrongway4you/Scripts/weather.sh" [] "weather" 18000,

    -- Date
    Run Date "<fc=#ee9a00>Date</fc>: %a %b %d %H:%M" "date" 600,

    -- Volume
    Run Com "/home/wrongway4you/Scripts/volume.sh" [] "vol" 100,

    -- Stdin
    Run StdinReader
  ],
  sepChar = "%",
  alignSep = "}{",
  template = " \
             \%StdinReader% \
             \}{\
             \%cpu% \
             \| %memory% * %swap% \
             \| %mpd% \
             \| <fc=#ee9a00>Weather</fc>: %weather% \
             \| %date% \
             \| <fc=#ee9a00>Vol</fc>: %vol%\
             \ "
}
