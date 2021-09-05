# -*- coding: utf-8 -*-
import os
import json
import re
import socket
import subprocess
from libqtile import qtile
from libqtile.config import Click, Drag, Group, KeyChord, Key, Match, Screen
from libqtile.command import lazy
from libqtile import layout, bar, widget, hook
from libqtile.lazy import lazy
from typing import List  # noqa: F401

# Settings
mod = "mod4"
myTerm = "alacritty"
myBrowser = "firefox"

barFontSize = 14
layout_theme = {"border_width": 1,
                "margin": 12,
                "border_focus": "e1acff",
                "border_normal": "1D2330"
                }

##################
### Workspaces ###
##################

group_names = [("WWW", {'layout': layout.MonadTall}),
               ("DEV", {'layout': layout.MonadTall}),
               ("SYS", {'layout': layout.MonadTall}),
               ("CHAT", {'layout': layout.MonadTall}),
               ("MUS", {'layout': layout.MonadTall}),
               ("GFX", {'layout': layout.Floating})]
groups = [Group(name, **kwargs) for name, kwargs in group_names]

###################
### Keybindings ###
###################

keys = [
         ### Layouts
         Key([mod, "shift"], "space",
             lazy.next_layout(),
             desc='Toggle through layouts'
             ),
         ### Media keys
         Key([], 'XF86Launch1', lazy.spawn('xlock')),
         Key([], 'XF86AudioMute', lazy.spawn('amixer -D pulse set Master toggle')),
         Key([], 'XF86AudioRaiseVolume', lazy.spawn('amixer -q sset Master 5%+')),
         Key([], 'XF86AudioLowerVolume', lazy.spawn('amixer -q sset Master 5%-')),

         ### Window navigation
         Key([mod], "j",
             lazy.layout.down(),
             desc='Move focus down in current stack pane'
             ),
         Key([mod], "k",
             lazy.layout.up(),
             desc='Move focus up in current stack pane'
             ),
         Key([mod], "h",
             lazy.layout.left(),
             lazy.layout.decrease_nmaster(),
             desc='Move focus left in the current stack pane'
             ),
         Key([mod], "l",
             lazy.layout.right(),
             lazy.layout.decrease_nmaster(),
             desc='Move focus left in the current stack pane'
             ),

         ### Move windows
         Key([mod, "shift"], "j",
             lazy.layout.shuffle_down(),
             lazy.layout.section_down(),
             desc='Move windows down in current stack'
             ),
         Key([mod, "shift"], "k",
             lazy.layout.shuffle_up(),
             lazy.layout.section_up(),
             desc='Move windows up in current stack'
             ),

         ### Window resizing
         Key([mod], "period",
             lazy.layout.grow(),
             lazy.layout.increase_nmaster(),
             desc='Expand window (MonadTall), increase number in master pane (Tile)'
             ),
         Key([mod], "comma",
             lazy.layout.shrink(),
             lazy.layout.decrease_nmaster(),
             desc='Shrink window (MonadTall), decrease number in master pane (Tile)'
             ),
         Key([mod], "n",
             lazy.layout.normalize(),
             desc='normalize window size ratios'
             ),
         Key([mod], "m",
             lazy.layout.maximize(),
             desc='toggle window between minimum and maximum sizes'
             ),

         ### Other window-related stuf
         Key([mod], "q",
             lazy.window.kill(),
             desc='Kill active window'
             ),
         Key([mod, "shift"], "f",
             lazy.window.toggle_floating(),
             desc='toggle floating'
             ),
         Key([mod], "f",
             lazy.window.toggle_fullscreen(),
             desc='toggle fullscreen'
             ),

         ### Apps
         Key([mod], "y",
             lazy.spawn(f"{os.environ['HOME']}/Scripts/make-screenshot.sh"),
             desc='Screenshot'
             ),
         Key([mod, "shift"], "n",
             lazy.spawn(f"{os.environ['HOME']}/Scripts/logout.sh"),
             desc='Logout'
             ),
         Key([mod], "Return",
             lazy.spawn(myTerm),
             desc='Launches My Terminal'
             ),
         Key([mod, "shift"], "Return",
             lazy.spawn("dmenu_run -p 'Run: '"),
             desc='Run Launcher'
             ),
         Key([mod], "b",
             lazy.spawn(myBrowser),
             desc='Browser'
             ),
         Key([mod, "shift"], "e",
             lazy.spawn("emacs"),
             desc='Emacs'
             ),
         ### Other
         Key([mod, "control"], "r",
             lazy.restart(),
             desc='Restart Qtile'
             ),
         Key([mod, "shift"], "q",
             lazy.shutdown(),
             desc='Shutdown Qtile'
             ),
         ### Treetab controls
         Key([mod, "shift"], "h",
             lazy.layout.move_left(),
             desc='Move up a section in treetab'
             ),
         Key([mod, "shift"], "l",
             lazy.layout.move_right(),
             desc='Move down a section in treetab'
             ),
         ### Stack controls
         Key([mod, "shift"], "Tab",
             lazy.layout.rotate(),
             lazy.layout.flip(),
             desc='Switch which side main pane occupies (XmonadTall)'
             ),
          Key([mod], "space",
             lazy.layout.next(),
             desc='Switch window focus to other pane(s) of stack'
             ),
#         Key([mod, "shift"], "space",
#             lazy.layout.toggle_split(),
#             desc='Toggle between split and unsplit sides of stack'
#             ),
         # Emacs programs launched using the key chord CTRL+e followed by 'key'
         KeyChord(["control"],"e", [
             Key([], "e",
                 lazy.spawn("emacsclient -c -a 'emacs'"),
                 desc='Launch Emacs'
                 ),
         ]),
         # Dmenu scripts launched using the key chord SUPER+p followed by 'key'
         KeyChord([mod], "p", [
             Key([], "e",
                 lazy.spawn("./dmscripts/dm-confedit"),
                 desc='Choose a config file to edit'
                 ),
             Key([], "i",
                 lazy.spawn("./dmscripts/dm-maim"),
                 desc='Take screenshots via dmenu'
                 ),
             Key([], "k",
                 lazy.spawn("./dmscripts/dm-kill"),
                 desc='Kill processes via dmenu'
                 ),
             Key([], "l",
                 lazy.spawn("./dmscripts/dm-logout"),
                 desc='A logout menu'
                 ),
             Key([], "m",
                 lazy.spawn("./dmscripts/dm-man"),
                 desc='Search manpages in dmenu'
                 ),
             Key([], "o",
                 lazy.spawn("./dmscripts/dm-bookman"),
                 desc='Search your qutebrowser bookmarks and quickmarks'
                 ),
             Key([], "r",
                 lazy.spawn("./dmscripts/dm-reddit"),
                 desc='Search reddit via dmenu'
                 ),
             Key([], "s",
                 lazy.spawn("./dmscripts/dm-websearch"),
                 desc='Search various search engines via dmenu'
                 ),
             Key([], "p",
                 lazy.spawn("passmenu"),
                 desc='Retrieve passwords with dmenu'
                 )
         ]),
         # Switch between workspaces
         *([ Key([mod], str(i), lazy.group[name].toscreen()) for i, (name, kwargs) in enumerate(group_names, 1) ]),
         # Send current window to workspace
         *([ Key([mod, "shift"], str(i), lazy.window.togroup(name)) for i, (name, kwargs) in enumerate(group_names, 1) ])
]

###################
### Layouts ###
###################

layouts = [
    layout.MonadTall(**layout_theme),
    layout.Max(**layout_theme),
    layout.Matrix(**layout_theme),
]

netInterface = "enp4s0"
colors = [["#282c34", "#282c34"], # panel background
          ["#3d3f4b", "#434758"], # background for current screen tab
          ["#ffffff", "#ffffff"], # font color for group names
          ["#ff5555", "#ff5555"], # border line color for current tab
          ["#74438f", "#74438f"], # border line color for 'other tabs' and color for 'odd widgets'
          ["#4f76c7", "#4f76c7"], # color for the 'even widgets'
          ["#e1acff", "#e1acff"], # window name
          ["#ecbbfb", "#ecbbfb"]] # backbround for inactive screens
# Fonts to use
boldFont = "Ubuntu Bold"
monoFont = "Ubuntu Mono"
iconFont = "Noto Color Emoji"
whiteColor = "#ffffff"
whiteColorA = [whiteColor, whiteColor]
# Background & foreground for the top bar
barBg = ["#282c34", "#282c34"]
barFg = ["#ffffff", "#ffffff"]
# Color for separators that divide left side widgets
sepColor = "#bebebe"
# Workspaces on the left
activeWsColor = whiteColorA
inactiveWsColor = "#ecbbfb"
winTitleColor = ["#eeeeee", "#eeeeee"]
widgetColor = whiteColorA
# Widgets
# Stripe config (1 = odd, 2 = even)
widgetStripedBg1 = "#4f76c7"
widgetStripedBg2 = "#74438f"
activeWsBg = ["#3d3f4b", "#434758"]
currWindowMaxChars = 77
thisCurrentScreenBorder = ["#e1acff", "#e1acff"]
thisScreenBorder = ["#74438f", "#74438f"]
otherScreenBorder = thisScreenBorder
otherCurrentScreenBorder = thisCurrentScreenBorder
# Widget paddings
wp = 10
inGroupPadding = wp

prompt = "{0}@{1}: ".format(os.environ["USER"], socket.gethostname())

widget_defaults = dict(
    font=monoFont,
    fontsize = barFontSize,
    padding = 0,
    background=barBg,
)
extension_defaults = widget_defaults.copy()

emptySep = widget.Sep(
    linewidth = 0,
    padding = wp,
    foreground = sepColor,
    background = barBg,
)

sep = widget.Sep(
    linewidth = 1,
    padding = 16,
    foreground = sepColor,
    background = barBg,
)

icons = {
    "volume": "",
    "ram": "",
    "cpu_temp": "",
    "weather": "",
}


env_config = {}
with open(f"{os.environ['HOME']}/.config/qtile/env.json", "r") as f:
    env_config = json.load(f)


def init_widgets_list():
    rightSideWidgets = [
         # Weather
         [{"icon": icons["weather"], "widget": lambda bg: widget.OpenWeather(
             app_key = env_config["OPEN_WEATHER_API_KEY"],
             cityid = 706483,
             background = bg,
             format = "{main_temp} °{units_temperature} {humidity}% {weather_details}",
         )}],
         # Thermal sensor (CPU)
         [{"icon": icons["cpu_temp"], "widget": lambda bg: widget.ThermalSensor(
             foreground = widgetColor,
             background = bg,
             threshold = 90,
             padding = 0,
         )}],
         [{"icon": icons["ram"], "widget": lambda bg: widget.Memory(
             foreground = widgetColor,
             background = bg,
             mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(myTerm + ' -e htop')},
             padding = 0,
             format = '{MemUsed: .0f}{mm} /{MemTotal: .0f}{mm}',
         )}],
        [{
            "icon": f"{icons['volume']}",
            "widget": lambda bg: widget.Volume(
                foreground = widgetColor,
                background = bg,
                padding = 0,
            ),
        }],
#        [{
#            "widget": lambda bg: widget.BitcoinTicker(
#                foreground = widgetColor,
#                background = bg,
#                padding = 0,
#            ),
#        }],
        [{
            "widget": lambda bg: widget.Pomodoro(
                color_active = widgetColor,
                color_break = widgetColor,
                color_inactive = widgetColor,
                foreground = widgetColor,
                background = bg,
                padding = 0,
                )
            }],
        # Music
#        [
#            {"widget": lambda bg: widget.Cmus(
#                play_color = widgetColor,
#                background = bg,
#                padding = 0,
#            )},
#        ],
        [
            {"widget": lambda bg: widget.Clock(
                foreground = widgetColor,
                background = bg,
                padding = 0,
                format = "%A, %B %d - %H:%M ",
            )},
        ],
        [{"widget": lambda bg: widget.CurrentLayoutIcon(
            custom_icon_paths = [os.path.expanduser("~/.config/qtile/icons")],
            foreground = barFg,
            background = bg,
            padding = 0,
            scale = 0.7
        ),}],
    ]


    icon_size = 12

    def renderStripedWidgetGroup(grp, even=False):
        res = []
        bg = widgetStripedBg2 if even else widgetStripedBg1
        res.append(
            widget.Sep(
                linewidth = 0,
                padding = wp,
                foreground = sepColor,
                background = bg,
            ),
        )
        for j, wdesc in enumerate(grp):
            text = wdesc.get("label", None)
            widget_fn = wdesc["widget"]
            has_text = text != None
            label_size = wdesc.get("label_fsize", 14)
            inner_padding = wdesc.get("inner_padding", 6)
            icon = wdesc.get("icon", None)
            has_icon = icon != None
            if has_text:
                res.append(
                    widget.TextBox(
                        text = text,
                        foreground = widgetColor,
                        background = bg,
                        padding = 0,
                        fontsize = label_size,
                    ),
                )
            elif has_icon:
                res.append(
                    widget.TextBox(
                        text = icon,
                        foreground = widgetColor,
                        background = bg,
                        padding = 0,
                        fontsize = icon_size,
                        font = "Font Awesome 5 Free",
                    ),
                )
                res.append(widget.Sep(
                        linewidth = 0,
                        padding = inner_padding,
                        foreground = sepColor,
                        background = bg,
                ))
            res.append(widget_fn(bg))
            if j != (len(grp) - 1):
                res.append(
                    widget.Sep(
                        linewidth = 0,
                        padding = inner_padding,
                        foreground = sepColor,
                        background = bg,
                    ),
                )
        res.append(
            widget.Sep(
                linewidth = 0,
                padding = wp,
                foreground = sepColor,
                background = bg,
            ),
        )
        return res
# Append rightSideWidgets with separators & alternating background color
    rightSide = []
    for i, grp in enumerate(rightSideWidgets):
        even = i % 2 == 0
        rightSide += renderStripedWidgetGroup(grp, even=even)

    widgets_list = [
        emptySep,
        # Python icon
        widget.Image(
            filename = "~/.config/qtile/icons/python-white.png",
            scale = True,
            margin_y = 3,
            mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(myTerm)},
        ),
        emptySep,
        # Workspaces
        widget.GroupBox(
            font = "Source Code Pro Bold",
            fontsize = 8,
            margin_y = 4,
            margin_x = 2,
            padding_y = 5,
            padding_x = 12,
            borderwidth = 2,
            active = activeWsColor,
            inactive = inactiveWsColor,
            disable_drag = True,
            rounded = False,
            highlight_color = activeWsBg,
            highlight_method = "line",
            this_current_screen_border = thisCurrentScreenBorder,
            this_screen_border = thisScreenBorder,
            other_current_screen_border = otherCurrentScreenBorder,
            other_screen_border = otherScreenBorder,
            foreground = barFg,
            background = barBg,
        ),
        widget.TextBox(
            text = "|>",
            foreground = barFg,
            background = barBg,
            padding = 8,
            fontsize = 14
        ),
        # Current window name
        widget.WindowName(
            foreground = winTitleColor,
            background = barBg,
            padding = 0,
            max_chars = currWindowMaxChars,
        ),
        # System tray
        widget.Systray(
            background = barBg,
            padding = 8,
            icon_size = 14,
        ),
        emptySep,
        emptySep,
        *rightSide
    ]
    return widgets_list


def init_widgets_screen1():
    widgets_screen1 = init_widgets_list()
    del widgets_screen1[7:8]               # Slicing removes unwanted widgets (systray) on Monitors 1,3
    return widgets_screen1


def init_screens():
    return [Screen(top=bar.Bar(widgets=init_widgets_screen1(), opacity=1.0, size=24))]


if __name__ in ["config", "__main__"]:
    screens = init_screens()
    widgets_list = init_widgets_list()
    widgets_screen1 = init_widgets_screen1()


def window_to_prev_group(qtile):
    if qtile.currentWindow is not None:
        i = qtile.groups.index(qtile.currentGroup)
        qtile.currentWindow.togroup(qtile.groups[i - 1].name)


def window_to_next_group(qtile):
    if qtile.currentWindow is not None:
        i = qtile.groups.index(qtile.currentGroup)
        qtile.currentWindow.togroup(qtile.groups[i + 1].name)


def window_to_previous_screen(qtile):
    i = qtile.screens.index(qtile.current_screen)
    if i != 0:
        group = qtile.screens[i - 1].group.name
        qtile.current_window.togroup(group)


def window_to_next_screen(qtile):
    i = qtile.screens.index(qtile.current_screen)
    if i + 1 != len(qtile.screens):
        group = qtile.screens[i + 1].group.name
        qtile.current_window.togroup(group)


def switch_screens(qtile):
    i = qtile.screens.index(qtile.current_screen)
    group = qtile.screens[i - 1].group
    qtile.current_screen.set_group(group)

# Mouse bindings

mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False

floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    # default_float_rules include: utility, notification, toolbar, splash, dialog,
    # file_progress, confirm, download and error.
    *layout.Floating.default_float_rules,
    Match(title='Confirmation'),      # tastyworks exit box
    Match(title='Qalculate!'),        # qalculate-gtk
    Match(wm_class='kdenlive'),       # kdenlive
    Match(wm_class='pinentry-gtk-2'), # GPG key password entry
    Match(wm_class='notification'),
    Match(wm_class='toolbar'),
    Match(wm_class='splash'),
    Match(wm_class='dialog'),
])
auto_fullscreen = True
focus_on_window_activation = "smart"

@hook.subscribe.startup_once
def start_once():
    home = os.path.expanduser('~')
    subprocess.call([home + '/.config/qtile/autostart.sh'])

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
