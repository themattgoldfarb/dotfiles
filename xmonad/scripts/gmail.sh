#!/bin/bash

#__xdotoolstring() {
  #inp="$1"
  #for (( i=0; i<${#inp}; i++ )); do
    #char="${inp:$i:1}"
    #ch=""
    #case $char in
      #' ')
        #ch="space";;
      #':')
        #ch="colon";;
      #'-')
        #ch="hyphen";;
      #*)
        #ch="$char";;
    #esac
    #echo -n "$ch"
    #xdotool key "$ch"
  #done
#}

filter="inbox"

if [[ $2 == "unread" ]]; then
  filter="unread"
fi

if [[ $1 == "--list" ]]; then

  echo 'annoying-stuff'
  echo 'arc'
  echo 'assistant-team'
  echo 'buganizer'
  echo 'direct-to-me'
  echo 'dogfood'
  echo 'eng-misc'
  echo 'finance'
  echo 'followup'
  echo 'footprints-team'
  echo 'forsale'
  echo 'fos-alerts'
  echo 'freshdocs'
  echo 'googlers'
  echo 'groups'
  echo 'gws'
  echo 'magic'
  echo 'misc'
  echo 'misc-ny'
  echo 'misc-tech'
  echo 'readability'
  echo 'video-games'
elif [[ $1 == "--xmonad" ]]; then
  xterm -e '~/.xmonad/scripts/gmail.sh --list | ~/.fzf/bin/fzf > /tmp/shellPrompt;'
  input="/tmp/shellPrompt"
  while IFS= read -r var
  do
    ~/.xmonad/scripts/gmail.sh $var $filter
  done < "$input"
else
  sleep .05;
  text="$filter label:$1"
  xdotool type "gl"
  sleep .05;
  xdotool type "$text"
  xdotool key Escape
  xdotool key KP_Enter
fi

