#!/bin/sh

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
	WRAPPER="ck-launch-session dbus-launch --exit-with-session --sh-syntax"

	export EDITOR="vim"
	export BROWSER="chromium"
	export LC_ALL="ja_JP.UTF-8"
#	export GTK_IM_MODULE="ibus"
#	export QT_IM_MODULE="ibus"
#	export XMODIFIERS=@im=ibus

	exec >> .wm.log

	xrdb -merge $erresources
	xrdb -merge $usrdefaults
	setxkbmap gb -option compose:ralt
	gtk-redshift -l 51.440794:-0.098963 -t 6500:3400 &
	dropboxd &
	DELAY=3 stagger ibus-daemon -x
}

startawesome ()
{
	xcompmgr &
	wicd-gtk -t &

	COMMAND="/usr/bin/awesome"
}

startdwm ()
{
	export GNOME_DESKTOP_SESSION_ID="01" # fool xdg-open
	export DE=gnome # fool dbus
	feh --bg-scale $HOME/.wallpaper

	echo -e "#-*- coding: utf-8 -*-\nSTATUS_COMMAND=['/bin/date', u'+%A %Od日 %Om月 %Y年 %EP%OI時%OM分%OS秒']\nSTATUS_UPDATE_INTERVAL=1.0\nDEFAULT_NOTIFY_TIMEOUT=5000\nMAX_NOTIFY_TIMEOUT=10000\nimport subprocess\ndef update_text(text):\n\tsubprocess.call(['xsetroot', '-name', text])" | statnot /dev/stdin &

	xcompmgr &
	wicd-gtk -t &
	ibamtray &
	conky | dzen2 -h 16 -y -1 -fn "Verdana:bold:size=9" &

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
    *) startdwm ;;
esac

exec $WRAPPER $COMMAND
