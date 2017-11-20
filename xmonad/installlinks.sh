#!/bin/bash

DIR=~/dotfiles/xmonad # dotfiles directory
OLD_DIR=~/xmonad_old # old dotfiles backup directory
DEST_DIR=~/.xmonad/

# list of files/folders to symlink in homedir
files="xmonad.hs xmobarmaster xmobarslave lib/ColorTheme.hs"

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
    echo "Moving any existing dotfiles from ~ to $OLD_DIR"
    [[ -e $DEST_DIR/$file ]] && mv $DEST_DIR/$file $OLD_DIR
    echo "Creating symlink to $file in $DEST_DIR."
    ln -s $DIR/$file $DEST_DIR/$file
done
