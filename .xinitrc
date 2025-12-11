#!/bin/sh
# eval $(dbus-launch --sh-syntax)
[ -f "$HOME/.xprofile" ] && . "$HOME/.xprofile"
if ! pgrep -f "feh" > /dev/null; then
    feh --bg-fill ~/wallpaper/tonybtw.png &
fi
bash ~/scripts/autostart &
bash ~/scripts/hdmi &
SERVICE_DIR="$HOME/.local/service"
if ! pgrep -f "runsvdir $SERVICE_DIR" > /dev/null; then
    runsvdir "$SERVICE_DIR" &
fi
if ! pgrep -f "flameshot" > /dev/null; then
    flameshot &
fi
if ! pgrep -f "picom" > /dev/null; then
    picom &
fi
exec oxwm
