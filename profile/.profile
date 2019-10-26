# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	      . "$HOME/.bashrc"
    fi
fi

if [ -d "$HOME/.pyenv/bin" ]; then
    PATH="${PATH:+"$PATH:"}$HOME/.pyenv/bin"
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
fi

pathappend() {
    for ARG in "$@"
    do
        if [ -d "$ARG" ] && [[ ":$PATH:" != *":$ARG:"* ]]; then
            PATH="${PATH:+"$PATH:"}$ARG"
        fi
    done
}

pathappend "$HOME/bin"
pathappend "$HOME/.local/bin"
pathappend "$HOME/.cargo/bin"
pathappend "$HOME/.poetry/bin"

export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src/"
