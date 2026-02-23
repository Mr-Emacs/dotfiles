# .bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
#export PS1='\u@\h:\w\$ '
export PS1='\[\e[32m\]\u@\h\[\e[34m\]:\w\[\e[0m\]\$ '
. "$HOME/.cargo/env"
export PATH="$HOME/.cargo/bin:$PATH"
