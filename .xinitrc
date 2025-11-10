eval $(dbus-launch --sh-syntax)
[ -f "$HOME/.xprofile" ] && . "$HOME/.xprofile"
feh --bg-fill ~/wallpaper/penger/wallpaper.jpeg &
bash ~/scripts/autostart &
bash ~/scripts/hdmi &
# flameshot &
#bash ~/scripts/slstatus &
bash ~/scripts/dwmbar &
# picom --config ~/Programming/xsoder/nwm/picom.conf &
#exec ~/dev/nwm/nwm
exec ~/Programming/xsoder/nwm/nwm
#exec ~/thirdparty/oxwm/target/release/oxwm
#exec i3
