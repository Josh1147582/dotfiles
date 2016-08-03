DIR=$(pwd)
cd ~

for i in vimrc vim zshrc zsh bashrc tmux.conf
do
    if [ -f .$i ]
    then
        rm -i .$i
    fi
    ln -s $DIR/$i .$i
done

mkdir -p .config/
if [ -d .config/nvim ]
then
    rm -ri .config/nvim
fi
ln -s $DIR/vim .config/nvim
