#!/bin/sh
eval $(dbus-launch --sh-syntax)
[ -f "$HOME/.xprofile" ] && . "$HOME/.xprofile"
if ! pgrep -f "feh" > /dev/null; then
    feh --bg-fill ~/Downloads/pexels-pixabay-33109.jpg
fi
bash ~/scripts/autostart &
bash ~/scripts/hdmi &
bash ~/scripts/network &
if ! pgrep -f "pulseaudio" > /dev/null; then
    pulseaudio &
fi
#jexec ~/Programming/xsoder/nwm/nwm
exec i3
