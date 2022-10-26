#!/bin/bash

SUFFIX="__desktop"
[[ -e $HOME/.laptop ]] && SUFFIX="__laptop"

DIR=~/dotfiles/xmonad # dotfiles directory
OLD_DIR=~/.xmonad_old # old dotfiles backup directory
DEST_DIR=~/.config/xmonad

# list of files/folders to symlink in homedir
files="xmonad.hs lib/ColorTheme.hs xmobarmaster_desktop xmobarslave_desktop \
       xmobarmaster_laptop xmobarslave_laptop lib/MyConfig.hs$SUFFIX"

###########

# create dotfiles_old in homedir
echo -n "Creating $OLD_DIR for backup of any existing dotfiles in ~ ..."
mkdir -p $OLD_DIR
echo "done"

mkdir -p $DEST_DIR

# change to the dotfiles directory
echo -n "Changing to the $DIR directory ..."
cd $DIR
echo "done"

for file in $files; do
    target=$file
    [[ $file =~ $SUFFIX ]] && target=${file:0:${#file}-${#SUFFIX}}
    echo "Moving any existing dotfiles from ~ to $OLD_DIR"
    [[ -e $DEST_DIR/$target ]] && mv $DEST_DIR/$target $OLD_DIR
    echo "Creating symlink to $file in $DEST_DIR."
    ln -s $DIR/$file $DEST_DIR/$target
done

