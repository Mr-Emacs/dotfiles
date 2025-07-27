# ------------------------------
# Minimal Custom .bashrc
# ------------------------------
# Core settings
export EDITOR=vim
export PATH=$PATH:$(go env GOPATH)/bin

# PATH setup

# Aliases
alias gitter="~/scripts/gitter.sh"
alias timer="bash ~/scripts/timer.sh"
alias harpoon="bash ~/scripts/tmux_harpoon.sh"

# Auto start X server on VT1
if [[ -z "$DISPLAY" && "$XDG_VTNR" -eq 1 ]]; then
    exec startx
fi
# ~/.bashrc

# Color variables
RED='\[\e[31m\]'
WHITE='\[\e[97m\]'
RESET='\[\e[0m\]'

update_prompt() {
    local exit_code=$?
    local cwd="\W"

    if [ $exit_code -ne 0 ]; then
        PS1="${RED}[\u${WHITE}@\h ${cwd}] ${RED}\\$ ${RESET}"
    else
        PS1="${RED}[\u${WHITE}@\h ${cwd}] \\$ ${RESET}"
    fi
}

PROMPT_COMMAND=update_prompt

alias luamake="/home/xsoder/packages/lua-language-server/3rd/luamake/luamake"
