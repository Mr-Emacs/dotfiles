#!/usr/bin/env bash

dunst &
#blueman-applet &
xclip -selection clipboard -loops 0 &
#bash $HOME/scripts/dwmbar &
rm -rf $HOME/.config/pulse 

#xrdb ~/.Xresources &
