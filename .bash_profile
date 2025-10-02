#!/usr/bin/env bash
FG="255;255;255"          
CURSOR="64;255;64"       
KEYWORD="205;149;12"    
CONSTANT="107;142;35"  
COMMAND="97;175;239"  
BRACKET="205;170;125"
INVALID="244;71;71" 

# function to get current git branch name
git_branch() {
    git rev-parse --abbrev-ref HEAD 2>/dev/null | awk '{print " ("$1")"}'
}

if [[ "$INSIDE_EMACS" == *vterm* ]]; then
    alias ls='ls --color=auto'
    PS1="\[\e[38;2;${CURSOR}m\]\u@\h\[\e[0m\]:"
    PS1+="\[\e[38;2;${KEYWORD}m\]\w\[\e[0m\]"
    PS1+="\[\e[38;2;255;0;0m\]\$(git_branch)\[\e[0m\]"
    PS1+="\[\e[38;2;${CONSTANT}m\]\$ \[\e[0m\]"
else
    alias ls='ls'
    RED='\[\e[38;5;196m\]'
    WHITE='\[\e[97m\]'
    RESET='\[\e[0m\]'
    cwd="\W"
    PS1="${RED}[\u${WHITE}@\h ${cwd}] ${WHITE}\\$ ${RESET}"
fi

. "$HOME/.cargo/env"
export XDG_RUNTIME_DIR="/run/user/$(id -u)"

if [[ -z "$DISPLAY" && "$(tty)" == "/dev/tty1" ]]; then
    exec startx
fi

