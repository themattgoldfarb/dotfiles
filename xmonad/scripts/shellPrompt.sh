#!/bin/bash

xterm -e '~/.xmonad/scripts/gmail.sh --list | fzf >> /tmp/shellPrompt'
input="/tmp/shellPrompt"
while IFS= read -r var
do
  ~/.xmonad/scripts/gmail.sh $var
done < "$input"
