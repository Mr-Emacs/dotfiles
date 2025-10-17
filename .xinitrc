eval $(dbus-launch --sh-syntax)
[ -f "$HOME/.xprofile" ] && . "$HOME/.xprofile"

feh --bg-fill /home/xsoder/wallpaper/master.png &
bash /home/xsoder/scripts/autostart &
#bash /home/xsoder/.dwm/autostart.sh &
pipewire &
wireplumber&
pipewire-pulse &
#picom --config ~/Programming/xsoder/nwm/picom.conf &
exec /home/xsoder/Programming/xsoder/nwm/nwm
#exec /home/xsoder/thirdparty/oxwm/target/release/oxwm
#exec /home/xsoder/.local/bin/oxwm
#exec i3
