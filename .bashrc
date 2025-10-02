source "$HOME"/.bash_profile
PROMPT_COMMAND='history -a'
export HISTTIMEFORMAT='%F %T '

export MANPAGER="less -R --use-color -Dd+r -Du+b"
export MANROFFOPT='-c'

export LD_LIBRARY_PATH="build:deps/raylib/lib"
export PATH="$HOME/scripts:$PATH"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
