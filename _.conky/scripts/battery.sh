#!/bin/bash

total=`cat /sys/class/power_supply/BAT0/charge_full_design`
rate=`cat /sys/class/power_supply/BAT0/charge_now`

if [[ $rate -eq 0 ]]; then
    echo '0 mW'
  else
    minutes=`echo "$rate / $total * 100 / 60" | bc -l`
    result=$(printf "%1.2f" $minutes)
    echo "$result % per minute"
fi
