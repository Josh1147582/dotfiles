# local zsh config
source ~/.zshrc.local

# History
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000

# Instantly write history
setopt -o sharehistory

# Ignore duplicates in history
setopt HIST_IGNORE_DUPS

# Delete old recorded entry if new entry is a duplicate.
setopt HIST_IGNORE_ALL_DUPS 

setopt appendhistory autocd extendedglob nomatch

bindkey -e

zstyle :compinstall filename '/home/josh/.zshrc'

autoload -Uz compinit
compinit -u

# List of word delimeters
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# ls pleasantness
alias l='\ls --color=auto'
alias ls='ls --color=auto'
alias la='ls -a --color=auto'
alias lsa='ls -a --color=auto'
alias lls='ls -lh --color=auto'
alias ll='ls -lh --color=auto'

# Use vim as the default text editor
export VISUAL=vim

# Home and End keysjump the the beginning/end of the command
bindkey "^[OH" beginning-of-line
bindkey "^[OF" end-of-line

# Ctrl-Left and Ctrl-Right keys move between words
bindkey ";5C" forward-word
# bindkey "^[[C" forward-word
bindkey ";5D" backward-word
# bindkey "^[[D" backward-word

# alias for vim muscle memory when quitting
alias :q='exit'

# eval used by thefuck
if hash fuck 2>/dev/null; then
    eval $(thefuck --alias)
fi

# timestamp alias for dates
alias ts='date +%y-%m-%d'

# default to terminal emacs
alias emacs='emacs -nw'

# alias for getting latest file in a folder
alias latest='ls -t | head -n 1'

# vi bindings
bindkey -v
export KEYTIMEOUT=1

bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^r' history-incremental-search-backward

function zle-line-init zle-keymap-select {
    VIM_PROMPT="%{$fg_bold[yellow]%} [% NORMAL]% %{$reset_color%}"
    RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/} $EPS1"
    zle reset-prompt
}
function zle-line-finish {
    VIM_PROMPT=""
    RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/} $EPS1"
    zle reset-prompt
}
RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/} $EPS1"

zle -N zle-line-init
zle -N zle-keymap-select

# grml
if [ ! $ZSH_CUSTOM ]
then
    # enable syntax highlighting
    source ~/.zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

    # enable C-s for forward search
    stty -ixon
fi

