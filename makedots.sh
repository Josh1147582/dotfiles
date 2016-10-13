#!/bin/bash -e

DIR=$(pwd)

#while getopts "ozgt:" opt; do
while getopts "ozt:" opt; do
    case "$opt" in
        o)  CONFIG="OHMYZSH"
            ;;
        z)  CONFIG="ZSH"
            ;;
#        g)  CONFIG="GRML"
#            ;;
        t)  TEMPLATE=$OPTARG
    esac
done

if [ ! $CONFIG ]
then
    echo "Please specify -o or -z for oh-my-zsh or standard zsh config."
    exit
fi

if [ ! $TEMPLATE ]
then
    echo "Pleas specify -t [template suffix]. for a local zsh config."
    exit
fi

# Template
if [ -f $HOME/.zshrc.local -o -h $HOME/.zshrc.local -o -d $HOME/.zshrc.local ]
then
    rm -i $HOME/.zshrc.local
fi
ln -s $DIR/zshrc.local.$TEMPLATE $HOME/.zshrc.local

if [ $CONFIG == "OHMYZSH" ]
then
    for i in zshrc oh.my.zsh zshrc.mine
    do
        if [ -f $HOME/.$i -o -h $HOME/.$i -o -d $HOME/.$i ]
        then
            rm -ri $HOME/.$i
        fi
    done
    ln -s $DIR/zshrc.oh.my.zsh $HOME/.zshrc
    ln -s $DIR/zshrc $HOME/.zshrc.mine
    ln -s $DIR/oh-my-zsh $HOME/.oh-my-zsh
fi

if [ $CONFIG == "ZSH" ]
then
    for i in zshrc zsh 
    do
        if [ -f $HOME/.$i -o -h $HOME/.$i -o -d $HOME/.$i ]
        then
            rm -ri $HOME/.$i
        fi
        ln -s $DIR/$i $HOME/.$i
    done
fi

for i in vimrc vim bashrc tmux.conf
do
if [ -f $HOME/.$i -o -h $HOME/.$i -o -d $HOME/.$i ]
    then
        rm -ri $HOME/.$i
    fi
    ln -s $DIR/$i $HOME/.$i
done

mkdir -p .config/
if [ -d .config/nvim ]
then
    rm -ri .config/nvim
fi
ln -s $DIR/vim $HOME/.config/nvim
