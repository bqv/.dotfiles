

# No multiplexing in emacs
if [ -z "$EMACS" ]; then;
 ## Launch in screen
 #[ -z "$WINDOW" ] && exec screen -xRR
 # Launch in tmux
 if [ -z "$TMUX" ]; then
  if [[ "$(tty)" != /dev/tty* ]]; then
      eval $(ssh-agent) > /dev/null
      tmux -q has-session && exec tmux attach || exec tmux new
  fi
 fi
else
 export LC_ALL='en_GB'
 export LANG='en_GB'
 export LC_CTYPE=C
 export SHELL="emacs $EMACS"
 #exec bash
fi

date

setopt NOHUP
#setopt NOTIFY
#setopt NO_FLOW_CONTROL
setopt INC_APPEND_HISTORY SHARE_HISTORY
setopt APPEND_HISTORY
# setopt AUTO_LIST		# these two should be turned off
# setopt AUTO_REMOVE_SLASH
# setopt AUTO_RESUME		# tries to resume command of same name
unsetopt BG_NICE		# do NOT nice bg commands
setopt CORRECT			# command CORRECTION
setopt EXTENDED_HISTORY		# puts timestamps in the history
# setopt HASH_CMDS		# turns on hashing
#
setopt MENUCOMPLETE
setopt ALL_EXPORT

# Set/unset  shell options
setopt   notify globdots correct pushdtohome cdablevars autolist
setopt   correctall autocd recexact longlistjobs
setopt   autoresume histignoredups pushdsilent 
setopt   autopushd pushdminus extendedglob rcquotes mailwarning
unsetopt bgnice autoparamslash
unsetopt correctall

# Autoload zsh modules when they are referenced
zmodload -a zsh/stat stat
zmodload -a zsh/zpty zpty
zmodload -a zsh/zprof zprof
zmodload -a zsh/mapfile mapfile


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
    PR_NO_COLOR="%{$terminfo[sgr0]%}"
#PS1="$PR_CYAN [$PR_BLUE$USER$PR_WHITE@$PR_GREEN$HOSTNAME$PR_NO_COLOR:$PR_RED%2c$PR_CYAN]$PR_YELLOW$screen#$PR_NO_COLOR " #COLORFUL
PS1="$PR_LIGHT_WHITE [$PR_WHITE$USER$PR_NO_COLOR@$PR_WHITE$HOSTNAME$PR_GREY:$PR_GREY%2c$PR_NO_COLOR$PR_LIGHT_WHITE]$PR_GREY$WINDOW#$PR_NO_COLOR " #COLORLESS
#RPS1="$PR_LIGHT_YELLOW(%D{%m-%d %H:%M})$PR_NO_COLOR" #COLORFUL
RPS1="$PR_WHITE(%D{%m-%d %H:%M})$PR_NO_COLOR" #COLORLESS
#LANGUAGE=
LC_ALL='en_GB.UTF-8'
LANG='en_GB.UTF-8'
#LC_ALL='ja_JP.UTF-8'
#LANG='ja_JP.UTF-8'
LC_CTYPE=C

source <(dircolors ~/.dir_colors)

if [ $SSH_TTY ]; then
  MUTT_EDITOR=vim
else
  MUTT_EDITOR=emacsclient.emacs-snapshot
fi

unsetopt ALL_EXPORT
# # --------------------------------------------------------------------
# # aliases
# # --------------------------------------------------------------------

alias slrn='slrn -n'
alias man='LC_ALL=C LANG=C man'
alias f="finger"
alias ll='ls -al'
alias ls="ls --color=auto"
alias offlineimap-tty='offlineimap -u TTY.TTYUI'
alias hnb-partecs='hnb $HOME/partecs/partecs-hnb.xml'
alias rest2html-css='rst2html --embed-stylesheet --stylesheet-path=/usr/share/python-docutils/s5_html/themes/default/print.css'
alias emacs='emacs -nw'
alias sh="PS1='\u \W\$ ' sh"
alias sudo='nocorrect sudo '
alias please='sudo'
alias fucking='sudo'
alias sorry='sudo !!'
alias cd..="cd .."
alias grep="grep --color=auto"
alias egrep="egrep --color=auto"
alias stat="/usr/bin/stat"
alias less="less -R"
alias diff="diff -s"
alias pacman="aura"
alias aura="sudo aura"
alias yaourt="sudo yaourt"
alias btrfs="sudo btrfs"
alias sstart="sudo systemctl start"
alias sstop="sudo systemctl stop"
alias srestart="sudo systemctl restart"
alias sstatus="sudo systemctl status"
alias senable="sudo systemctl enable"
alias sdisable="sudo systemctl disable"
alias smask="sudo systemctl mask"
alias sunmask="sudo systemctl unmask"
alias sreload="sudo systemctl daemon-reload"
alias gtop="intel_gpu_top"
alias push="ADB_TRACE=adb adb push"
alias feh="feh -F"

# # --------------------------------------------------------------------
# # functions
# # --------------------------------------------------------------------

function pm() {
    { aura --color=always -Ss $1 && aura -As $1 } | less
}

function dotify() {
    mv $1 .dotfiles/_$1
    ln -s .dotfiles/_$1 $1
}

matrix() { cmatrix -a -b -s; }
hexscroll() { echo -e '\033[0;32m'; pv -qL 90 /dev/urandom | xxd -c $(( ($COLUMNS-10)*2/7 )); }

autoload -U compinit
compinit
bindkey -e
bindkey "^?" backward-delete-char
bindkey '^[[3~' delete-char
bindkey '^[OH' beginning-of-line
bindkey '^[[1~' beginning-of-line
bindkey '^[[7~' beginning-of-line
bindkey '^[OF' end-of-line
bindkey '^[[4~' end-of-line
bindkey '^[[8~' end-of-line
bindkey '^[[5~' up-line-or-history
bindkey '^[[6~' down-line-or-history
bindkey "^L" clear-screen
bindkey "^r" history-incremental-search-backward
bindkey ' ' magic-space    # also do history expansion on space
bindkey '^I' complete-word # complete on tab, leave expansion to _expand
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path ~/.zsh/cache/$HOST

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'
zstyle ':completion:*' menu select=1 _complete _ignored _approximate
zstyle -e ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/2 )) numeric )'
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'

# Completion Styles

# list of completers to use
zstyle ':completion:*::::' completer _expand _complete _ignored _approximate

# allow one error for every three characters typed in approximate completer
zstyle -e ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/2 )) numeric )'
    
# insert all expansions for expand completer
zstyle ':completion:*:expand:*' tag-order all-expansions

# formatting and messages
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
zstyle ':completion:*' group-name ''

# match uppercase from lowercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# offer indexes before parameters in subscripts
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# command for process lists, the local web server details and host completion
# on processes completion complete all user processes
zstyle ':completion:*:processes' command 'ps -au$USER'

## add colors to processes for kill completion
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'

zstyle ':completion:*:processes' command 'ps ax -o pid,s,nice,stime,args | sed "/ps/d"'
zstyle ':completion:*:*:kill:*:processes' command 'ps --forest -A -o pid,user,cmd'
zstyle ':completion:*:processes-names' command 'ps axho command' 
#zstyle ':completion:*:urls' local 'www' '/var/www/htdocs' 'public_html'
#
#NEW completion:
# 1. All /etc/hosts hostnames are in autocomplete
# 2. If you have a comment in /etc/hosts like #%foobar.domain,
#    then foobar.domain will show up in autocomplete!
zstyle ':completion:*' hosts $(awk '/^[^#]/ {print $2 $3" "$4" "$5}' /etc/hosts | grep -v ip6- && grep "^#%" /etc/hosts | awk -F% '{print $2}') 
# Filename suffixes to ignore during completion (except after rm command)
zstyle ':completion:*:*:(^rm):*:*files' ignored-patterns '*?.o' '*?.c~' \
    '*?.old' '*?.pro'
# LaTeX suffixes to ignore during vi and vim completion
zstyle ':completion::*:(vi|vim|emacs):*' ignored-patterns \
	'*?.(aux|dvi|log|idx|pdf|rel|out)'

# ignore completion functions (until the _ignored completer)
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:*:*:users' ignored-patterns \
        adm adb apache bin daemon games gdm halt ident junkbust lp mail mailnull \
        named news nfsnobody nobody nscd ntp operator pcap postgres radvd \
        rpc rpcuser rpm shutdown squid sshd sync uucp vcsa xfs avahi-autoipd\
        avahi backup messagebus beagleindex debian-tor dhcp dnsmasq fetchmail\
        firebird gnats haldaemon hplip irc klog list man cupsys postfix\
        proxy syslog www-data mldonkey sys snort
# SSH Completion
zstyle ':completion:*:scp:*' tag-order \
   files users 'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
zstyle ':completion:*:scp:*' group-order \
   files all-files users hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' tag-order \
   users 'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
zstyle ':completion:*:ssh:*' group-order \
   hosts-domain hosts-host users hosts-ipaddr
zstyle '*' single-ignored show

fortune -as linux linuxcookie paradoxum computers science definitions | tee -a /tmp/fortune.log | cowsay
echo -e '\n' >> /tmp/fortune.log

