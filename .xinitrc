eval $(dbus-launch --sh-syntax)
[ -f "$HOME/.xprofile" ] && . "$HOME/.xprofile"
feh --bg-fill /home/xsoder/wallpaper/master.png &
bash /home/xsoder/scripts/autostart &
bash /home/xsoder/scripts/hdmi &
pipewire &
wireplumber &
pipewire-pulse &
flameshot &
#picom --config ~/Programming/xsoder/nwm/picom.conf &
exec ~/Programming/xsoder/nwm/nwm
#exec i3
#exec ~/thirdparty/oxwm/target/release/oxwm
