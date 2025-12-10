#!/bin/sh
# eval $(dbus-launch --sh-syntax)
[ -f "$HOME/.xprofile" ] && . "$HOME/.xprofile"
if ! pgrep -f "feh" > /dev/null; then
    feh --bg-fill ~/wallpaper/anime/wp11814616-kita-ikuyo-wallpapers.jpg &
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
# if ! pgrep -f "blueman-applet" > /dev/null; then
#     blueman-applet &
# fi
# bash ~/scripts/dwmbar &
# exec ~/Programming/xsoder/nwm/nwm
exec i3
# For the other Window manager
# if ! pgrep -u "$USER" -x emacs >/dev/null 2>&1; then
#     emacs --daemon
# fi
# exec ~/thirdparty/oxwm/target/release/oxwm
# setxkbmap -option altwin:meta_alt
# exec emacsclient -a "" -c
