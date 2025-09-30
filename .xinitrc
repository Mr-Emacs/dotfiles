eval $(dbus-launch --sh-syntax)
[ -f "$HOME/.xprofile" ] && . "$HOME/.xprofile"

feh --bg-fill /home/xsoder/wallpaper/penger/3dmaze.jpg &
bash /home/xsoder/scripts/autostart &
pulseaudio &
exec i3
