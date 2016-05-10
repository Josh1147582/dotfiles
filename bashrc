PS1='\[\e[1;32m\]\u@\h \W\$ \[\e[0m\]'

# ls pleasantness
alias l='\ls --color=auto'
alias ls='ls -a --color=auto'
alias lls='ls -alh --color=auto'

# grep the entirety of the history
alias hg='history | grep'

# Use vim as the default text editor
export VISUAL=vim

# local folder in PATH 
export PATH=$HOME/bin:$PATH

# Flags used for CS243 "Mechanics of Programming"
alias gcc='gcc -std=c99 -Wall -ggdb -Wextra -pedantic'

# Functions for converting between hex and decimal
h2d(){
    echo "ibase=16; $@"|bc
  }
d2h(){
    echo "obase=16; $@"|bc
}

# alias for vim muscle memory when quitting
alias :q='exit'
alias :e='vim'

# allow X forwarding in Cygwin
if [ $(uname -o) == "Cygwin" ]; then
    export DISPLAY=:0
fi
