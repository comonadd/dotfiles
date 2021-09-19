#!/bin/bash

RT=$(realpath ".")

ln -s "$RT/.bashrc.common" ~/.config/.bashrc.common
ln -s "$RT/.bashrc.linux" ~/.bashrc
