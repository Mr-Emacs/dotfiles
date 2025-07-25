if ! pgrep -u "$USER" ssh-agent > /dev/null; then
	eval "$(ssh-agent -s)"
	ssh-add ~/.ssh/id_rsa_github
else
	. ~/.ssh/agent.env
fi

pulseaudio --start
exec dwm
