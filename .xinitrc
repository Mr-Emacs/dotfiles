[ -f "$HOME/.xprofile" ] && . "$HOME/.xprofile"
eval $(dbus-launch --sh-syntax)

bash /home/xsoder/scripts/wallpaper &
pulseaudio --start
exec dwm
