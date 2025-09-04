PROMPT_COMMAND='history -a'
export HISTTIMEFORMAT='%F %T '
if [[ -z "$DISPLAY" && "$XDG_VTNR" -eq 1 ]]; then
    exec startx
fi

export MANPAGER="less -R --use-color -Dd+r -Du+b"
export MANROFFOPT='-c'

PS1='\[\033[97m\]\u@\h:\w\$ \[\033[0m\]'

export LD_LIBRARY_PATH="build:deps/raylib/lib"
