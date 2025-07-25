[ -f "$HOME/.xprofile" ] && . "$HOME/.xprofile"

pulseaudio --start
exec dwm

