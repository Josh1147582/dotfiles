#!/bin/bash

# Rename a workspace, keeping its current number, so $mod+$num still works.
# If nothing is given, remove the name.

current_ws_num=$(i3-msg -t get_workspaces | jq '.[] | select(.focused==true).num'| cut -d"\"" -f2)
new_name=$(i3-input -P 'New name: ' | grep 'command = ' | cut -d" " -f3)

if [ -z "$new_name" ]; then
	i3-msg "rename workspace to $current_ws_num"
else
	i3-msg "rename workspace to \"$current_ws_num $new_name\""
fi
