#!/bin/bash

__laptop() {
  xmodmap -e "keycode 66 = Caps_Lock NoSymbol Caps_Lock" 
  xmodmap -e "keycode 9 = Escape NoSymbol Escape"   
  
  xmodmap -e "remove Lock = Caps_Lock"
  xmodmap -e "keysym Caps_Lock = Escape"
  
  xmodmap -e "keycode 9 = Caps_Lock"   
  xmodmap -e "keycode 66 = Escape"   
}

__desktop() {
  xmodmap -e "keycode 9 = Caps_Lock NoSymbol Caps_Lock" 
  xmodmap -e "keycode 66 = Escape NoSymbol Escape"   
  
  xmodmap -e "remove Lock = Caps_Lock"
  xmodmap -e "keysym Caps_Lock = Escape"
  
  xmodmap -e "keycode 66 = Caps_Lock"   
  xmodmap -e "keycode 9 = Escape"   
}

if [[ $1 == "desktop" ]]; then
  __desktop
else
  __laptop
fi


