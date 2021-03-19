# local zsh config
if [ -f ~/.zshrc.local ]; then
    source ~/.zshrc.local
fi

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
