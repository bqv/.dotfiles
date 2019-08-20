#!/usr/bin/zsh

PERL_SIGNALS=unsafe
PATH="./:$PATH:/bin/:/sbin/:/usr/bin/:/usr/sbin/:/usr/local/bin/:/usr/local/sbin:$HOME/bin/"
TZ="Europe/London"
HISTFILE=$HOME/.zhistory
HISTSIZE=20000
SAVEHIST=20000
HOSTNAME="`hostname`"
PAGER='less'
EDITOR='vim'
ALTERNATE_EDITOR='emacsclient'
DAY="`date | cut -b 1`"

autoload colors zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
    colors
fi
for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE GREY; do
    eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
    eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
    (( count = $count + 1 ))
done

if [ -n "$TMUX_PANE" ]; then
    cycle=($PR_RED $PR_YELLOW $PR_GREEN $PR_CYAN $PR_BLUE $PR_MAGENTA $PR_GREY $PR_LIGHT_WHITE)
    PR_RANDOM=${cycle[$((${TMUX_PANE:1}%7))]}
else
    PR_RANDOM=$PR_GREY
fi
PR_NO_COLOR="%{$terminfo[sgr0]%}"
function old_precmd() {
    RET=$?
    if [ $RET != 0 ]; then
        PR_RET=${cycle[$((($RET-1)%6+1))]}
    else
        PR_RET=$PR_WHITE
    fi
    ROW=${cycle[$(($HISTCMD%5+1))]}
    PS1="$ROW:$PR_LIGHT_WHITE [$PR_WHITE$USER$PR_RANDOM@$PR_WHITE$HOSTNAME$PR_GREY:$PR_GREY%2c$PR_NO_COLOR$PR_WHITE]$PR_RET$RET#$PR_NO_COLOR " #COLORLESS
    RPS1="$PR_WHITE(%D{%m-%d %H:%M})$PR_NO_COLOR" #COLORLESS
    PS2=" $PR_GREY:$PR_WHITE -$PR_NO_COLOR%_$PR_WHITE> "
    PS3="?# "
    PS4="+ "
}

LC_ALL='en_GB.UTF-8'
LANG='en_GB.UTF-8'
LC_CTYPE=C

if [ -f "~/.dir_colors" ]; then
    source <(dircolors ~/.dir_colors)
fi

if [ $SSH_TTY ]; then
  MUTT_EDITOR=vim
else
  MUTT_EDITOR=emacsclient
fi
