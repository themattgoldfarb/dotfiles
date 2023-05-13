## Stuff to install
* Font Awesome - Used for icons in my XMobar config
```
  sudo apt install fonts-font-awesome
```

## Install XMonad and XMobar using cabal
### Install Dependencies
```
sudo apt install libx11-dev libxft-dev libxinerama-dev libxrandr-dev \
                libxss-dev libxpm-dev xorg-dev libpango1.0-dev \
                libasound2-dev libmpd-dev
```

### Install cabal
```
sudo apt install cabal-install
```

### Install XMonad and XMobar
```
cabal update
cabal install xmonad 
cabal install --lib xmonad-contrib
cabal install xmobar --flags="all_extensions" --overwrite-policy=always
```

## Useful Links
* [Install XMonad](https://xmonad.org/INSTALL.html)
  * I like using the cabal install method.

## Other Stuff
### Make Ctrl+Alt+Backspace kill X Server
```
sudo dpkg-reconfigure keyboard-configuration
```

Press enter a couple times until you get to the screen about Control+Alt+Backspace and change that to `<Yes>`
