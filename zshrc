# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob nomatch
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/josh/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# Use prompt colors (green for regular user, red for root)
autoload -U colors && colors
if [ "$(id -u)" != "0" ]; then
    PROMPT="%{$fg_bold[green]%}%n@%m %1~ $%{$reset_color%} "
    RPROMPT="[%{$fg_no_bold[yellow]%}%?%{$reset_color%}]"
else
    PROMPT="%{$fg_bold[red]%}%n@%m %1~ #%{$reset_color%} "
    RPROMPT="[%{$fg_no_bold[yellow]%}%?%{$reset_color%}]"
fi

# ls pleasantness
alias l='\ls --color=auto'
alias ls='ls --color=auto'
alias lsa='ls -a --color=auto'
alias lls='ls -alh --color=auto'

# apt-get aliases 
alias update='sudo apt-get update && sudo apt-get dist-upgrade' 
alias clean='sudo apt-get clean'

# grep the entirety of the history
alias hg='history 1 | grep'

# Use vim as the default text editor
export VISUAL=vim

# Used to keep steam from throwing errors
export SWT_GTK3=0

# Home and End keysjump the the beginning/end of the command
bindkey "^[OH" beginning-of-line
bindkey "^[OF" end-of-line

# Ctrl-Left and Ctrl-Right keys move between words
bindkey ";5C" forward-word
bindkey ";5D" backward-word

# bind UP and DOWN arrow keys
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# Used for X forwarding in Cygwin
if [[ "$(uname -o)" != "GNU/Linux" ]]; then
    DISPLAY=:1.0
fi

# local folder in PATH for UE4
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

# devkitPro/devkitPPC sources
export DEVKITPRO=/home/josh/devkitPro/ 
export DEVKITPPC=/home/josh/devkitPro//devkitPPC

# alias for vim muscle memory when quitting
alias :q='exit'

# eval used by thefuck
if hash fuck 2>/dev/null; then
    eval $(thefuck --alias)
fi

# zsh-syntax-highlighting (MUST BE AT THE BOTTOM OF THIS FILE)
source ~/.zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# zsh-history-substring-search (MUST BE BELOW SYNTAX HIGHLIGHTING)
source ~/.zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
