#!/bin/bash

DIR=~/dotfiles                    # dotfiles directory
OLD_DIR=~/dotfiles_old             # old dotfiles backup directory
XMONAD_SCRIPTS=~/.xmonad/scripts

# list of files/folders to symlink in homedir
files="vimrc ideavimrc tmux.conf \
       xmobarrc xmobarrc2 xmobarrc_single \
       arrowkeyremap Xmodmap xprofile Xresources \
       xmodmaprc"

declare -A SPECIAL
SPECIAL=(
    #['xmonad.hs']="$HOME/.xmonad/"
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

for file in $files; do
    echo "Moving any existing dotfiles from ~ to $OLD_DIR"
    [[ -e $HOME/.$file ]] && mv ~/.$file $OLD_DIR
    echo "Creating symlink to $file in home directory."
    ln -s $DIR/$file ~/.$file
done

for file in ${!SPECIAL[@]} ; do
    dest="${SPECIAL[$file]}"
    if [[ -e "$dest/$file" ]] ; then
        echo "Backing up $dest/$file to $OLD_DIR"
        mv "$dest/$file" $OLD_DIR
    fi
    echo "Creating symlink to $file in $dest"
    ln -s $DIR/$file $dest/$file
done

for file in $DIR/xmonadscripts/*; do
    echo $file
    if [[ -e "$XMONAD_SCRIPTS/$file" ]] ; then
        echo "Backing up $dest/$file to $OLD_DIR"
        mv "$XMONAD_SCRIPTS/$file" $OLD_DIR
    fi
    echo "Creating symlink to $file in xmonad scripts"
    [[ ! -d $XMONAD_SCRIPTS ]] && mkdir $XMONAD_SCRIPTS
    ln -s $file $XMONAD_SCRIPTS/${file##*/}
done

mkdir -p $HOME/.config/nvim
if [[ -e "$HOME/.config/nvim/init.vim" ]] ; then
    mv "$HOME/.config/nvim/init.vim" $OLD_DIR
fi
ln -s $HOME/.vimrc $HOME/.config/nvim/init.vim

$DIR/desktopFiles/installDesktopFiles.sh
$DIR/iconFiles/installIconFiles.sh
$DIR/otherFiles/installlinks.sh
$DIR/xmonad/installlinks.sh
