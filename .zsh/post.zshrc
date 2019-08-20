#!/usr/bin/zsh

source ~/.zsh/agkozak-zsh-prompt/agkozak-zsh-prompt.plugin.zsh
AGKOZAK_BLANK_LINES=1
AGKOZAK_MULTILINE=0
#AGKOZAK_PROMPT_CHAR=( $ %# : )
AGKOZAK_PROMPT_CHAR=( ❯ ❯ ❮ )
AGKOZAK_COLORS_PROMPT_CHAR='red'
AGKOZAK_COLORS_PATH='green'
AGKOZAK_CUSTOM_SYMBOLS=( '⇣⇡' '⇣' '⇡' '+' 'x' '!' '>' '?' )
AGKOZAK_USER_HOST_DISPLAY=0
AGKOZAK_CUSTOM_RPROMPT+=' %F{32}%*'

if [ -x "$(command -v cowsay)" ]; then
    if [ -x "$(command -v fortune)" ]; then
        fortune -as linux linuxcookie paradoxum computers science definitions | tee -a /tmp/fortune.log | cowsay
        echo -e '\n' >> /tmp/fortune.log
    else
        echo "fortune not installed :(" | cowsay
    fi
fi
