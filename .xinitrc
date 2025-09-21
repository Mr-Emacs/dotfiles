eval $(dbus-launch --sh-syntax)
[ -f "$HOME/.xprofile" ] && . "$HOME/.xprofile"

bash /home/xsoder/scripts/wallpaper &
bash /home/xsoder/scripts/autostart &
pulseaudio --start &
exec i3
