#!/usr/bin/env bash
FG="255;255;255"
CURSOR="64;255;64"
KEYWORD="205;149;12"
CONSTANT="107;142;35"
COMMAND="97;175;239"
BRACKET="205;170;125"
INVALID="244;71;71"
# git_branch() {
#     git rev-parse --abbrev-ref HEAD 2>/dev/null | awk '{print " ("$1")"}'
# }

# if [[ "$INSIDE_EMACS" == *vterm* ]]; then
#     alias ls='ls --color=auto'
#     PS1="\[\e[38;2;${CURSOR}m\]\u@\h\[\e[0m\]:"
#     PS1+="\[\e[38;2;${KEYWORD}m\]\w\[\e[0m\]"
#     PS1+="\[\e[38;2;255;0;0m\]\$(git_branch)\[\e[0m\]"
#     PS1+="\[\e[38;2;${CONSTANT}m\]\$ \[\e[0m\]"
# else
alias ls='ls --color=auto'
BLUE='\e[0;34m'
WHITE='\[\e[97m\]'
RESET='\[\e[0m\]'
cwd="\W"
export PS1="${WHITE}[\h${BLUE}@\u ${WHITE}${cwd}] ${WHITE}\\$ ${RESET}"
# fi

export XDG_RUNTIME_DIR="/run/user/$(id -u)"

if [ -z "$DBUS_SESSION_BUS_ADDRESS" ]; then
    eval "$(dbus-launch --sh-syntax --exit-with-session)"
fi
if [[ -z "$DISPLAY" && "$(tty)" == "/dev/tty1" ]]; then
    exec startx
fi
#export XDG_SESSION_TYPE=wayland
#export MOZ_ENABLE_WAYLAND=1
#if [[ -z "$DISPLAY" && "$(tty)" == "/dev/tty1" ]]; then
#    exec sway
#fi
#if [[ -z "$DISPLAY" && "$(tty)" == "/dev/tty1" ]]; then
#    exec hyprland
#fi
#. "$HOME/.cargo/env"
# Only run fastfetch in real terminal sessions
# if [[ $- == *i* ]] && [ -t 0 ] && [ -z "$INSIDE_EMACS" ] && [ -z "$EMACS" ]; then
#     fastfetch
# fi
