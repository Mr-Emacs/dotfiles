eval $(dbus-launch --sh-syntax)
[ -f "$HOME/.xprofile" ] && . "$HOME/.xprofile"
feh --bg-fill ~/wallpaper/penger/win-emacs.png &
bash ~/scripts/autostart &
bash ~/scripts/hdmi &
SERVICE_DIR="$HOME/runit/service"
if ! pgrep -f "runsvdir $SERVICE_DIR" > /dev/null; then
    runsvdir "$SERVICE_DIR" &
fi
flameshot &
#bash ~/scripts/slstatus &
bash ~/scripts/dwmbar &
# picom --config ~/Programming/xsoder/nwm/picom.conf &
#exec ~/dev/nwm/nwm
exec ~/Programming/xsoder/nwm/nwm
#exec ~/thirdparty/oxwm/target/release/oxwm
#exec i3
