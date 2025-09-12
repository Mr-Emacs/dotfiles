source "$HOME"/.bash_theme
PROMPT_COMMAND='history -a'
export HISTTIMEFORMAT='%F %T '

if [[ -z "$DISPLAY" && "$(tty)" == "/dev/tty1" ]]; then
    exec startx
fi

export MANPAGER="less -R --use-color -Dd+r -Du+b"
export MANROFFOPT='-c'

export PATH="$HOME/scripts $PATH"
alias ls='ls --color=auto'
