if [ -f "/usr/local/dcs/lib/config/Bash_Profile" ]; then
    source /usr/local/dcs/lib/config/Bash_Profile
fi

PS1='\[\e[1;32m\]\u@\h \W\$ \[\e[0m\]'
