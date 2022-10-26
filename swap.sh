#!/bin/bash

DESKTOP_FILE="$HOME/.localenv/keydesktop"

__laptop() {
  xmodmap -e "keycode 66 = Caps_Lock NoSymbol Caps_Lock" 
  xmodmap -e "keycode 9 = Escape NoSymbol Escape"   
  
  xmodmap -e "remove Lock = Caps_Lock"
  xmodmap -e "keysym Caps_Lock = Escape"
  
  xmodmap -e "keycode 9 = Caps_Lock NoSymbol Caps_Lock" 
  xmodmap -e "keycode 66 = Escape NoSymbol Escape"   
  [[ -e $DESKTOP_FILE ]] && rm $DESKTOP_FILE

  xrdb -merge "$HOME/.xres/hidpi.xres"
}

__desktop() {
  xmodmap -e "keycode 9 = Caps_Lock NoSymbol Caps_Lock" 
  xmodmap -e "keycode 66 = Escape NoSymbol Escape"   
  
  xmodmap -e "remove Lock = Caps_Lock"
  xmodmap -e "keysym Caps_Lock = Escape"
  
  xmodmap -e "keycode 66 = Caps_Lock NoSymbol Caps_Lock" 
  xmodmap -e "keycode 9 = Escape NoSymbol Escape"   
  [[ ! -e $DESKTOP_FILE ]] && touch $DESKTOP_FILE

  xrdb -merge "$HOME/.xres/lowdpi.xres"
}


echo "$KEYBOARD_MODE"

if [[ $1 == "desktop" ]]; then
  __desktop
elif [[ $1 == "laptop" ]]; then
  __laptop
elif [[ $1 == "swap" ]]; then
  if [[ -e "$DESKTOP_FILE" ]]; then
    __laptop
  else
    __desktop
  fi
else
  __laptop
fi


