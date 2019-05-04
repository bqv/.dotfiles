#!/usr/bin/zsh

# No multiplexing in emacs
if [ -z "$EMACS" ]; then
    if [ -n "$DISPLAY" ]; then                      # If we're in X11
        [ -z "$TMUX" ] && exec tmux new -A -s $(sed 's/X:/X/;s/:/-/' <<< X$DISPLAY)
    elif [[ "$(tty)" == /dev/tty* ]]; then          # If we're in TTY
        [ -z "$WINDOW" ] && exec screen -xRR
    elif [ -n "$SSH_CONNECTION" ]; then             # If we're in SSH
        [ -z "$TMUX" ] && exec tmux new -A -s ssh-$USER
    else
        tmux ls
	read -n
    fi
else
    export LC_ALL='en_GB'
    export LANG='en_GB'
    export LC_CTYPE=C
    export SHELL="emacs $EMACS"
fi

date

