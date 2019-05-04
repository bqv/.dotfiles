#!/usr/bin/zsh

function pacsearch() {
    { aura --color=always -Ss $@ && aura -As $@ } | less -X
}

function sprunge() {
    test -z $1 && FILE='-' || FILE=$1

    curl -sF "sprunge=<${FILE}" http://sprunge.us
}

function clearresetsource() {
    clear; reset; source ~/.zshrc;
}

function please() {
    if [ -z "$@" ]; then
        sudo $(fc -ln -1)
    else
        sudo $@
    fi
}

function push() {
    TargetDir=$2
    FILE=$1

    if [ "$TargetDir" -eq "" ]; then
        TargetDir="/sdcard"
    fi
    SIZE=$( stat -c %s $FILE )
    file=$( basename $FILE )
    echo "Pushing $FILE to $TargetDir/$file"
    trap "break" CHLD
    coproc ( set +m
             adb push $FILE $TargetDir/$file 2> /dev/null || \
             echo "Error: failed"
             set -m
           )
    while sleep 1; do
        SZ=$( adb shell stat -t $TargetDir/$file | sed "s%^$TargetDir/$file \([0-9]\+\).\+%\1%" )
        echo | awk '{printf "\r[ %5.1f%% ] ( %s/%s )",('$SZ'/'$SIZE')*100,'$SZ'/1024,'$SIZE'/1024}'
        if [ $SZ -eq $SIZE ]; then
            break
        fi
    done
    echo ""
}
