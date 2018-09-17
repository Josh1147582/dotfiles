# local zsh config
source ~/.zshrc.local

export PATH=$PATH:$HOME/bin
export PATH=$PATH:$HOME/.local/bin

# load antigen if it exists
if [ -h ~/.antigen.zsh ]
then

    save_aliases=$(alias -L)

    source ~/.antigen.zsh

    # Load oh-my-zsh's library.
    antigen use oh-my-zsh

    # Bundles from the default repo (robbyrussell's oh-my-zsh).
    antigen bundle git
    antigen bundle heroku
    antigen bundle pip
    antigen bundle lein
    antigen bundle command-not-found

    # Syntax highlighting bundle.
    antigen bundle zsh-users/zsh-syntax-highlighting

    antigen bundle zsh-users/zsh-autosuggestions

    # Load the theme.
    antigen theme bureau

    # Tell antigen that you're done.
    antigen apply

    unalias -m '*'
    eval $save_aliases; unset save_aliases

fi

bindkey '^ ' autosuggest-accept

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

# List of word delimeters
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# ls pleasantness
alias l='\ls --color=auto'
alias ls='ls --color=auto'
alias la='ls -a --color=auto'
alias lsa='ls -a --color=auto'
alias lls='ls -lh --color=auto'
alias ll='ls -lh --color=auto'

# Custom ls colors
eval $(dircolors ~/.dircolors)

# Use emacsclient as the default text editor
export EDITOR="emacsclient -t"
export VISUAL="emacsclient -c -a emacs"

# eval used by thefuck
if hash fuck 2>/dev/null; then
    eval $(thefuck --alias)
fi

# Home and End keys jump the the beginning/end of the command
bindkey "^[OH" beginning-of-line
bindkey "^[[H" beginning-of-line
bindkey "^[OF" end-of-line
bindkey "^[[F" end-of-line

# Enable forward search
bindkey "^s" history-incremental-search-forward

# Ctrl-Left and Ctrl-Right keys move between words
bindkey ";5C" forward-word
bindkey ";5D" backward-word

# cdls
function cd {
    builtin cd "$@" && ls -F
}

function sl_func() {
    # sl - prints a mirror image of ls. (C) 2017 Tobias Girstmair, https://gir.st/, GPLv3
    #
    LEN=$(ls "$@" |wc -L) # get the length of the longest line

    ls "$@" | rev | while read -r line
    do
      printf "%${LEN}.${LEN}s\\n" "$line" | sed 's/^\(\s\+\)\(\S\+\)/\2\1/'
      done

}

alias sl=sl_func

# vi bindings
#bindkey -v
#export KEYTIMEOUT=1
#
#bindkey '^P' up-history
#bindkey '^N' down-history
#bindkey '^?' backward-delete-char
#bindkey '^h' backward-delete-char
#bindkey '^w' backward-kill-word
#bindkey '^r' history-incremental-search-backward
#
#function zle-line-init zle-keymap-select {
#    VIM_PROMPT="%{$fg_bold[yellow]%} [% NORMAL]% %{$reset_color%}"
#    RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/} $EPS1"
#    zle reset-prompt
#}
#function zle-line-finish {
#    VIM_PROMPT=""
#    RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/} $EPS1"
#    zle reset-prompt
#}
#RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/} $EPS1"
#
#zle -N zle-line-init
#zle -N zle-keymap-select
