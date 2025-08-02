[ -f "$HOME/.xprofile" ] && . "$HOME/.xprofile"
export XCURSOR_THEME=DMZ-White
export XCURSOR_SIZE=28
xsetroot -cursor_name left_ptr &

pulseaudio --start
exec dwm
