DIR=$(pwd)
cd ~
rm -i .vimrc
ln -s $DIR/vimrc .vimrc
rm -i .vim
ln -s $DIR/vim .vim

rm -i .zshrc
ln -s $DIR/zshrc .zshrc
rm -i .zsh
ln -s $DIR/zsh .zsh

rm -i .bashrc
ln -s $DIR/bashrc .bashrc
