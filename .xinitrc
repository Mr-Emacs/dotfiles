eval $(dbus-launch --sh-syntax)
[ -f "$HOME/.xprofile" ] && . "$HOME/.xprofile"
feh --bg-fill /home/xsoder/wallpaper/master.png &
bash /home/xsoder/scripts/autostart &
pipewire &
wireplumber &
pipewire-pulse &
#exec i3
picom --config ~/Programming/xsoder/nwm/picom.conf &
exec ~/Programming/xsoder/nwm/nwm
#exec ~/thirdparty/oxwm/target/release/oxwm
