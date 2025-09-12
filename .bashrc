source "$HOME"/.bash_profile
PROMPT_COMMAND='history -a'
export HISTTIMEFORMAT='%F %T '

if [[ -z "$DISPLAY" && "$(tty)" == "/dev/tty1" ]]; then
    exec startx
fi

export MANPAGER="less -R --use-color -Dd+r -Du+b"
export MANROFFOPT='-c'

export PATH="$HOME/scripts $PATH"
export LD_LIBRARY_PATH="build:deps/raylib/lib"
