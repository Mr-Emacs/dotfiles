pulseaudio --start
exec dwm
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_rsa_github
