#!/bin/sh
eval $(dbus-launch --sh-syntax)
[ -f "$HOME/.xprofile" ] && . "$HOME/.xprofile"
if ! pgrep -f "feh" > /dev/null; then
    feh --bg-fill ~/dotfiles/wallpaper.png
fi
bash ~/scripts/autostart &
bash ~/scripts/hdmi &
bash ~/scripts/network &
bash ~/scripts/dwmbar &
if ! pgrep -f "pulseaudio" > /dev/null; then
    pulseaudio &
fi
exec ~/Programming/xsoder/nwm/nwm
