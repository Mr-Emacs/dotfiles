# Core settings
export EDITOR=vim

PROMPT_COMMAND='history -a'
export HISTTIMEFORMAT='%F %T '
# Auto start X server on VT1
if [[ -z "$DISPLAY" && "$XDG_VTNR" -eq 1 ]]; then
    exec startx
fi

RED='\[\e[31m\]'
WHITE='\[\e[97m\]'
RESET='\[\e[0m\]'

update_prompt() {
    local exit_code=$?
    local cwd="\W"
    local venv=""
    if [ -n "$VIRTUAL_ENV" ]; then
        venv="($(basename "$VIRTUAL_ENV")) "
    fi
    PS1="${WHITE}${venv}\u${RED}@\h${WHITE} ${cwd} ${WHITE}\\$ ${RESET}"
}
PROMPT_COMMAND=update_prompt

export MANPAGER="less -R --use-color -Dd+r -Du+b"
export MANROFFOPT='-c'

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

export LD_LIBRARY_PATH="build:deps/raylib/lib"
. "$HOME/.cargo/env"
