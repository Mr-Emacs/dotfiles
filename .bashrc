# .bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
BLUE='\e[0;34m'
WHITE='\[\e[97m\]'
RESET='\[\e[0m\]'
cwd="\W"
PS1="${WHITE}[\h${BLUE}@\u ${WHITE}${cwd}] ${WHITE}\\$ ${RESET}"
. "$HOME/.cargo/env"

export NVM_DIR="$HOME/.config/nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
