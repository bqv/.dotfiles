#!/usr/bin/zsh

fortune -as linux linuxcookie paradoxum computers science definitions | tee -a /tmp/fortune.log | cowsay
echo -e '\n' >> /tmp/fortune.log

