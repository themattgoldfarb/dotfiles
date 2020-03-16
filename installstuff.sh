#!/bin/bash

echo "Checking vim plug."
if [[ -e "$HOME/.vim/autoload/plug.vim" ]] ; then
    echo "Vim plug already exists."
else
    echo "Installing vim plug"
    curl -flo ~/.vim/autoload/plug.vim --create-dirs \
          https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

echo "Checking nvim symlink"
if [[ -e "$HOME/.config/nvim" ]] ; then
    echo "nvim already linked"
else
    echo "Setting up symlink"
    ln -s "$HOME/.vim" "$HOME/.config/nvim"
fi

packages=("xdotool")
for pkg in "${packages[@]}"; do
  if dpkg --get-selections | grep "$pkg" > /dev/null ; then
    echo "$pkg is already installed."
  else
    echo -n "Installing $pkg..."
    sudo apt-get --assume-yes install $pkg
    echo "done."
  fi
done

