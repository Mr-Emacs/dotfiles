eval $(dbus-launch --sh-syntax)
[ -f "$HOME/.xprofile" ] && . "$HOME/.xprofile"

feh --bg-fill /home/xsoder/wallpaper/master.png &
bash /home/xsoder/scripts/autostart &
pipewire &
pipewire-pulse &
wireplumber& 
exec i3
