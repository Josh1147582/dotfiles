if [ -f "/usr/local/dcs/lib/config/Bash_Profile" ]; then
    source /usr/local/dcs/lib/config/Bash_Profile
fi

PS1='\[\e[1;32m\]\u@\h \W\$ \[\e[0m\]'

# ls pleasantness
alias l='\ls --color=auto'
alias ls='ls --color=auto'
alias la='ls -a --color=auto'
alias lsa='ls -a --color=auto'
alias lls='ls -lh --color=auto'
alias ll='ls -lh --color=auto'
# Custom ls colors
eval $(dircolors ~/.dircolors)

# grep the entirety of the history
alias hg='history | grep'

# Use vim as the default text editor
export VISUAL=vim

# local folder in PATH
export PATH=$HOME/bin:$PATH

# Terminal Emacs
alias emacs-term='\emacs -nw --color=no'

# alias for vim muscle memory when quitting
alias :q='exit'
