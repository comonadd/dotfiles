Config {
--    font = "xft:Ubuntu Mono-14"
       font = "xft:Ubuntu Mono:weight=bold:pixelsize=14:antialias=true:hinting=true"
       , additionalFonts = [ "xft:Mononoki Nerd Font:pixelsize=13:antialias=true:hinting=true" ]
       , borderColor = "black"
       , border = NoBorder
       , bgColor = "#282c34"
       , fgColor = "#ffffff"
       , alpha = 255
       -- TODO: Specify only height
       , position = Static { xpos = 0, ypos = 0, width = 1920, height = 28 }
       , textOffset = -1
       , iconOffset = -1
       , pickBroadest = False
       , persistent = True
       , lowerOnStart = True
       , hideOnStart = False
       , allDesktops = True
       , overrideRedirect = True
       , iconRoot = "/home/comonadd/.xmonad/xpm/"
       , commands = [
                     Run Weather "UKHH"
                                 ["-t","<station>: <tempC>C",
                                  "-L","18","-H","25",
                                  "--normal","green",
                                  "--high","red",
                                  "--low","lightblue"] 36000

                    , Run Network "enp4s0" ["-t","<dev>: <fn=1>\xf6d9</fn> <rx>KBps <fn=1>\xfa51</fn> <tx>KBps"
                                            , "-L", "0", "-H", "8388608"
                                            , "--normal", "darkgreen", "--high", "darkred"] 10
                    , Run Cpu ["-L","3","-H","50",
                               "--normal","darkgreen","--high","darkred"] 10

                    , Run MultiCoreTemp  [ "--template" , "Temp: <core0>°C & <core1>°C"
                                         , "--Low"      , "70"        -- units: °C
                                         , "--High"     , "80"        -- units: °C
                                         , "--low"      , "darkgreen"
                                         , "--normal"   , "darkorange"
                                         , "--high"     , "darkred"
                                         ] 50

                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Swap [] 10
                    -- Hostname & username
                    , Run Com "uname" ["-n"] "" 36000
                    , Run Com "whoami" [] "" 36000
                    -- Volume
                    , Run Volume "default" "Master" [ "--template", "<status>Vol: <volume>%"
                                                    ] 10
                    -- Time and date
                    , Run Date "%a %b%_d %Y %H:%M:%S" "date" 10

                    , Run UnsafeStdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " <icon=haskell_20.xpm/> <fc=#666>|</fc> %UnsafeStdinReader% <fc=#666>|</fc> %cpu% <fc=#666>|</fc> %multicoretemp% <fc=#666>|</fc> %memory% <> %swap% <fc=#666>|</fc> %enp4s0% }{ <fc=#82AAFF>%date%</fc> <fc=#666>|</fc> %default:Master% <fc=#666>|</fc> %UKHH% <fc=#666>|</fc> <fc=#82AAFF>%whoami%</fc>@%uname% "
       }
