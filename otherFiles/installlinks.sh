#!/bin/bash

DIR=~/dotfiles/otherFiles          # dotfiles directory
OLD_DIR=~/dotfiles_old             # old dotfiles backup directory

declare -A SPECIAL
SPECIAL=(
    ['WindowPropertiesRE.hs']="$HOME/.xmonad/lib/XMonad/Util"
)

###########

# create dotfiles_old in homedir
echo -n "Creating $OLD_DIR for backup of any existing dotfiles in ~ ..."
mkdir -p $OLD_DIR
echo "done"

# change to the dotfiles directory
echo -n "Changing to the $DIR directory ..."
cd $DIR
echo "done"

for file in ${!SPECIAL[@]} ; do
    dest="${SPECIAL[$file]}"
    mkdir -p "$dest"
    if [[ -e "$dest/$file" ]] ; then
        echo "Backing up $dest/$file to $OLD_DIR"
        mv "$dest/$file" $OLD_DIR
    fi
    echo "Creating symlink to $file in $dest"
    ln -s $DIR/$file $dest/$file
done
