# Core settings
export EDITOR=vim

PROMPT_COMMAND='history -a'
export HISTTIMEFORMAT='%F %T '
# Auto start X server on VT1
if [[ -z "$DISPLAY" && "$XDG_VTNR" -eq 1 ]]; then
    exec startx
fi

export MANPAGER="less -R --use-color -Dd+r -Du+b"
export MANROFFOPT='-c'

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

. "$HOME/.cargo/env"
export LD_LIBRARY_PATH="build:deps/raylib/lib"
alias token='/home/xsoder/scripts/token'
