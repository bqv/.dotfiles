#!/bin/sh

###OBSOLETE###

erresources=$HOME/.Xresources
usrdefaults=$HOME/.Xdefaults
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

stagger ()
{
	sleep $DELAY
	exec $@ &
}

init ()
{
	WRAPPER="dbus-launch --exit-with-session --sh-syntax"

	export EDITOR="vim"
	export BROWSER="chromium"
	export LC_ALL="ja_JP.UTF-8"
	export GTK_IM_MODULE="fcitx"
	export QT_IM_MODULE="fcitx"
	export XMODIFIERS="@im=fcitx"

#	exec >> .wm.log #bleh

	xrdb -merge $erresources
	xrdb -merge $usrdefaults
	x11vnc -autoport 8644 -passwd onyx -auth guess -forever -oa /var/log/vnc/x11vnc.log &
	redshiftgui &
	dropboxd &
	udiskie &
	DELAY=3 stagger fcitx -x
}

startawesome ()
{
	xcompmgr &
	nm-applet &

	COMMAND="/usr/bin/awesome"
}

startdwm ()
{
	feh --bg-scale $HOME/.wallpaper

	echo -e "#-*- coding: utf-8 -*-\nSTATUS_COMMAND=['/bin/date', u'+%A %Od日 %Om月 %Y年 %EP%OI時%OM分%OS秒']\nSTATUS_UPDATE_INTERVAL=1.0\nDEFAULT_NOTIFY_TIMEOUT=5000\nMAX_NOTIFY_TIMEOUT=10000\nimport subprocess\ndef update_text(text):\n\tsubprocess.call(['xsetroot', '-name', text])" | statnot /dev/stdin &

	xcompmgr &
	nm-applet &
	ibamtray &
	conky | dzen2 -h 16 -y -1 -fn "Verdana:bold:size=9" &

	setxkbmap gb -option compose:ralt
	COMMAND="/usr/bin/dwm"
}

startconsole ()
{
	chvt 2
	exit
}

init

case "$1" in
	console) startconsole ;;
    dwm) startdwm ;;
	awesome) startawesome ;;
    openbox) COMMAND="openbox-session" ;;
    fluxbox) COMMAND="startfluxbox" ;;
    xfce4) COMMAND="startxfce4" ;;
    KDE4|kde4) COMMAND="startkde" ;;
    gnome) COMMAND="gnome-session" ;;
    wmii) COMMAND="wmii" ;;
#   e17) COMMAND="/usr/bin/enlightenment_start" ;;
#   pekwm) COMMAND="/usr/bin/pekwm" ;;
#   ratpoison) COMMAND="ratpoison" ;;
    twm) COMMAND="/usr/bin/twm" ;;
    *) COMMAND="$1" ;;
esac

exec $WRAPPER $COMMAND
