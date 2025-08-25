[ -f "$HOME/.xprofile" ] && . "$HOME/.xprofile"
eval $(dbus-launch --sh-syntax)

bash /home/xsoder/scripts/wallpaper &
bash /home/xsoder/.dwm/autostart.sh &
pulseaudio --start
exec i3
