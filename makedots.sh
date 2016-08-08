DIR=$(pwd)

for i in vimrc vim zshrc zsh bashrc tmux.conf
do
    if [ -f $HOME/.$i ]
    then
        rm -i $HOME/.$i
    fi
    ln -s $DIR/$i $HOME/.$i
done

mkdir -p .config/
if [ -d .config/nvim ]
then
    rm -ri .config/nvim
fi
ln -s $DIR/vim .config/nvim
