#!/bin/bash
# @raycast.schemaVersion 1
# @raycast.title Emacs Launcher
# @raycast.mode silent
# @raycast.packageName Emacs
# @raycast.icon 🚀

# Bind a global hotkey to this Raycast script command.  It opens launcher
# in a popup frame (see ../launcher-popup.el); the frame closes after a
# choice or C-g, and cancelling gives the focus back to the previous app.

EC=/opt/homebrew/bin/emacsclient

# Start a daemon when none is running.
$EC -e t >/dev/null 2>&1 || /opt/homebrew/bin/emacs --daemon

$EC -n -e '(my/launcher-popup)'
