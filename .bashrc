# .bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
BLUE='\e[0;34m'
WHITE='\[\e[97m\]'
RESET='\[\e[0m\]'
cwd="\W"
PS1="${WHITE}[\h${BLUE}@\u ${WHITE}${cwd}] ${WHITE}\\$ ${RESET}"
