#!/bin/bash


echo "Checking vim plug."
if [[ -e "$HOME/.vim/autoload/plug.vim" ]] ; then
    echo "Vim plug already exists."
else
    echo "Installing vim plug"
    curl -flo ~/.vim/autoload/plug.vim --create-dirs \
          https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi
