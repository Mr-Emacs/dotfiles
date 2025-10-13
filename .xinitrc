eval $(dbus-launch --sh-syntax)
[ -f "$HOME/.xprofile" ] && . "$HOME/.xprofile"

feh --bg-fill /home/xsoder/wallpaper/master.png &
bash /home/xsoder/scripts/autostart &
#bash /home/xsoder/.dwm/autostart.sh &
pipewire &
pipewire-pulse &
wireplumber&
#picom --config ~/Programming/xsoder/nwm/picom.conf &
#exec ~/thirdparty/oxwm/target/release/oxwm
#exec /home/xsoder/Programming/xsoder/nwm/nwm
exec i3
