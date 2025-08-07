# Core settings
export EDITOR=vim
export PATH=$PATH:$(go env GOPATH)/bin

# Auto start X server on VT1
if [[ -z "$DISPLAY" && "$XDG_VTNR" -eq 1 ]]; then
    exec startx
fi
# Color variables
RED='\[\e[31m\]'
WHITE='\[\e[97m\]'
RESET='\[\e[0m\]'

update_prompt() {
    local exit_code=$?
    local cwd="\W"

    if [ $exit_code -ne 0 ]; then
        PS1="${WHITE}[\u${RED}@\h ${cwd}] ${WHITE}\\$ ${RESET}"
    else
        PS1="${WHITE}[\u${RED}@\h ${WHITE}${cwd}] ${WHITE}\\$ ${RESET}"
    fi
}
export TERM=xterm-256color
PROMPT_COMMAND=update_prompt
