#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

if [ -x "$(command -v zsh)" ]; then
    #exec zsh $@
fi

HISTSIZE= 
HISTFILESIZE=

alias ls='ls --color=auto'
alias please='sudo'
alias fucking='sudo'
alias sorry='sudo !!'
alias cd..="cd .."
alias grep="grep --color=auto"
alias egrep="egrep --color=auto"
alias stat="/usr/bin/stat"
alias less="less -R"
alias diff="diff -s"
alias sstart="sudo systemctl start"
alias sstop="sudo systemctl stop"
alias srestart="sudo systemctl restart"
alias sstatus="sudo systemctl status"
alias senable="sudo systemctl enable"
alias sdisable="sudo systemctl disable"
alias smask="sudo systemctl mask"
alias sunmask="sudo systemctl unmask"
alias sreload="sudo systemctl daemon-reload"

PS1='[\u@\h \W]\$ '

PS1="\n\[\e[1;30m\][$$:$PPID - \j:\!\[\e[1;30m\]]\[\e[0;36m\] \T \[\e[1;30m\][\[\e[1;34m\]\u@\H\[\e[1;30m\]:\[\e[0;37m\]${SSH_TTY:-o} \[\e[0;32m\]+${SHLVL}\[\e[1;30m\]] \[\e[1;37m\]\w\[\e[0;37m\] \n\$ "
