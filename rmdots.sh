#!/bin/bash

DIR=$(pwd)

for i in vimrc vim bashrc tmux.conf zshrc emacs.d dircolors compton.conf .config/nvim .antigen.zsh .xmonad .xmobarrc .local/share/applications/emacs-client.desktop
do
	if [ -L $HOME/$i ]
	then
		rm -i $HOME/$i
	fi
done
