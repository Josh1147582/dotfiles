#!/bin/bash

DIR=$(pwd)

# Choose only the template
if [ ! $1 ]
then
    echo "Please specify [template suffix]. for a local zsh config."
    exit
fi


# Install Template
if [ -f $HOME/.zshrc.local -o -h $HOME/.zshrc.local -o -d $HOME/.zshrc.local ]
then
    rm -i $HOME/.zshrc.local
fi
ln -s $DIR/zshrc.local.$1 $HOME/.zshrc.local


# Install all dotfiles
if [ -f $HOME/.zshrc -o -h $HOME/.zshrc -o -d $HOME/.zshrc ]
then
    rm -ri $HOME/.zshrc
fi
ln -s $DIR/zshrc $HOME/.zshrc

for i in vimrc vim bashrc tmux.conf zshrc emacs.d dircolors compton.conf
do
if [ -f $HOME/.$i -o -h $HOME/.$i -o -d $HOME/.$i ]
    then
        rm -ri $HOME/.$i
    fi
    ln -s $DIR/$i $HOME/.$i
done


# Nvim
mkdir -p $HOME/.config/
if [ -d $HOME/.config/nvim ]
then
    rm -ri $HOME/.config/nvim
fi
ln -s $DIR/vim $HOME/.config/nvim


# Antigen
if [ -h $HOME/.antigen.zsh ]
then
    rm -i $HOME/.antigen.zsh
fi
ln -s $DIR/antigen/antigen.zsh $HOME/.antigen.zsh

# Xmonad
if [ -h $HOME/.xmonad ]
then
    rm -i $HOME/.xmonad
fi
ln -s $DIR/xmonad $HOME/.xmonad

if [ -h $HOME/.xmobarrc ]
then
    rm -i $HOME/.xmobarrc
fi
ln -s $DIR/xmonad/xmobarrc $HOME/.xmobarrc
