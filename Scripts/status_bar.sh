#!/bin/sh
# File: status_cmd.sh
# Creation date: 2017-02-28
# Creator: Dmitry Guzeev <dmitry.guzeev@yahoo.com>
# Description:
# The status command file for the i3 WM

echo '{"version":1}[[],'
exec conky -c $HOME/.conkyrc
