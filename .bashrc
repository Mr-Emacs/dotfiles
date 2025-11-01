source "$HOME"/.bash_profile
PROMPT_COMMAND='history -a'
export HISTTIMEFORMAT='%F %T '

export MANPAGER="less -R --use-color -Dd+r -Du+b"
export MANROFFOPT='-c'

export LD_LIBRARY_PATH="/home/xsoder/opt/raylib-5.5_linux_amd64/lib"
export PATH="$HOME/scripts:$PATH"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"
. "$HOME/.cargo/env"
