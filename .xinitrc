#!/bin/sh
eval $(dbus-launch --sh-syntax)
[ -f "$HOME/.xprofile" ] && . "$HOME/.xprofile"
if ! pgrep -f "feh" > /dev/null; then
    feh --bg-fill ~/wallpaper/penger/PengerWorks.png &
fi
bash ~/scripts/autostart &
bash ~/scripts/hdmi &
bash ~/scripts/network &
if ! pgrep -f "pulseaudio" > /dev/null; then
    pulseaudio &
fi
exec i3
