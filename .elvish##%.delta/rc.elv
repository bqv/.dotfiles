E:GOPATH = ~/go
paths = [
  ~/bin
  $E:GOPATH/bin
  /usr/local/bin
  /usr/local/sbin
  /usr/sbin
  /sbin
  /usr/bin
  /bin
]
 
use epm
 
epm:install &silent-if-installed=$true   \
github.com/zzamboni/elvish-modules     \
github.com/zzamboni/elvish-completions \
github.com/zzamboni/elvish-themes      \
github.com/xiaq/edit.elv               \
github.com/muesli/elvish-libs          \
github.com/iwoloschin/elvish-packages
 
use re
 
use readline-binding
 
edit:insert:binding[Alt-Backspace] = $edit:kill-small-word-left~
 
#edit:insert:binding[Alt-d] = $edit:kill-small-word-right~
 
use github.com/zzamboni/elvish-modules/alias
 
alias:new ls e:exa --color-scale --git --group-directories-first
alias:new more less
 
use github.com/xiaq/edit.elv/smart-matcher
smart-matcher:apply
 
use github.com/zzamboni/elvish-completions/vcsh
use github.com/zzamboni/elvish-completions/cd
use github.com/zzamboni/elvish-completions/ssh
use github.com/zzamboni/elvish-completions/builtins
 
use github.com/zzamboni/elvish-completions/git
git:init
 
use github.com/zzamboni/elvish-completions/comp
 
use github.com/zzamboni/elvish-themes/chain
chain:bold-prompt = $true
 
chain:segment-style = [
  &dir=          session
  &chain=        session
  &arrow=        session
  &git-combined= session
]
 
chain:glyph[git-ahead]  = "⬆ "
chain:glyph[git-staged] = "✔ "
 
edit:prompt-stale-transform = { each [x]{ styled $x[text] "gray" } }
edit:-prompt-eagerness = 10
edit:rprompt-persistent = $true

row = 0
fn coloured [idx s]{
    if (eq $idx 0) {
        put (styled $s red);
    } elif (eq $idx 1) {
        put (styled $s green);
    } elif (eq $idx 2) {
        put (styled $s yellow);
    } elif (eq $idx 3) {
        put (styled $s blue);
    } elif (eq $idx 4) {
        put (styled $s magenta);
    } elif (eq $idx 5) {
        put (styled $s cyan);
    } elif (eq $idx 6) {
        put (styled $s white);
    } else {
        put (styled $s gray);
    }
}
fn rainbow [s]{
    n = (% $row 7);
    put (coloured $n $s);
}
fn randcolor [s]{
    try {
        pane = $E:TMUX_PANE[1];
        put (coloured $pane $s);
    } except e {
        put (styled $s gray);
    }
}

pane = (randcolor "@")
edit:prompt = {
    row = (+ $row 1);
    put (rainbow ":");
    put (edit:styled " [" white);
    put (edit:styled (whoami) white);
    put $pane;
    put (edit:styled (hostname) white);
    put (edit:styled ":" gray);
    put (edit:styled (tilde-abbr $pwd) gray);
    put (edit:styled "]" white);
    put (edit:styled "0" gray);
    put (edit:styled "# " white);
}
edit:rprompt = {
    put (edit:styled "(" white);
    put (edit:styled (date +"%m-%d %H:%M") white);
    put (edit:styled ")" white);
}
 
use github.com/zzamboni/elvish-modules/long-running-notifications
 
use github.com/zzamboni/elvish-modules/bang-bang
 
use github.com/zzamboni/elvish-modules/dir
alias:new cd &use=[github.com/zzamboni/elvish-modules/dir] dir:cd
alias:new cdb &use=[github.com/zzamboni/elvish-modules/dir] dir:cdb
 
edit:insert:binding[Alt-i] = $dir:history-chooser~
 
edit:insert:binding[Alt-b] = $dir:left-small-word-or-prev-dir~
edit:insert:binding[Alt-f] = $dir:right-small-word-or-next-dir~
 
use github.com/zzamboni/elvish-modules/terminal-title
 
private-loaded = ?(use private)
 
use github.com/zzamboni/elvish-modules/atlas
 
use github.com/zzamboni/elvish-modules/opsgenie
 
use github.com/zzamboni/elvish-modules/leanpub
 
E:LESS = "-i -R"
 
E:EDITOR = "vim"
 
E:LC_ALL = "en_GB.UTF-8"
 
use github.com/zzamboni/elvish-modules/util
 
use github.com/muesli/elvish-libs/git
 
use github.com/iwoloschin/elvish-packages/update
update:curl-timeout = 3
update:check-commit &verbose

alias:new ls e:exa --color=auto
alias:new grep e:grep --color=auto
alias:new egrep e:egrep --color=auto
alias:new stat e:stat
alias:new diff e:diff -s
alias:new sstart e:sudo systemctl start
alias:new sstop e:sudo systemctl stop
alias:new srestart e:sudo systemctl restart
alias:new sstatus e:sudo systemctl status
alias:new senable e:sudo systemctl enable
alias:new sdisable e:sudo systemctl disable
alias:new smask e:sudo systemctl mask
alias:new sunmask e:sudo systemctl unmask
alias:new sreload e:sudo systemctl daemon-reload
alias:new ustart e:systemctl start --user
alias:new ustop e:systemctl stop --user
alias:new urestart e:systemctl restart --user
alias:new ustatus e:systemctl status --user
alias:new uenable e:systemctl enable --user
alias:new udisable e:systemctl disable --user
alias:new umask e:systemctl mask --user
alias:new uunmask e:systemctl unmask --user
alias:new ureload e:systemctl daemon-reload --user
 
-exports- = (alias:export)

fortune -as linux linuxcookie paradoxum computers science definitions | tee -a /tmp/fortune.log | cowsay
echo -e '\n' >> /tmp/fortune.log
 
