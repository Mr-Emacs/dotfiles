eval $(dbus-launch --sh-syntax)
[ -f "$HOME/.xprofile" ] && . "$HOME/.xprofile"
feh --bg-fill ~/wallpaper/penger/smooth_pengernal.jpg &
bash ~/scripts/autostart &
bash ~/scripts/hdmi &
flameshot &
# picom --config ~/Programming/xsoder/nwm/picom.conf &
#exec ~/Programming/xsoder/nwm/nwm
#exec ~/thirdparty/oxwm/target/release/oxwm
exec i3
