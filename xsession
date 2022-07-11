#!/bin/sh

# Source .profile for common environment vars
. ~/.profile

# Disable access control for the current user
xhost +SI:localuser:$USER

# Start Shepherd to manage user daemons
if [ -z "$(pgrep -u me shepherd)" ]; then
  shepherd
fi

# Restoring soundcards settings
sudo alsactl --file ~/.guix/asound.state restore

# Uncomment this to start xterm instead for debugging purposes!
# Then you can manually run the window manager and log output
# > exec dbus-launch emacs -mm --debug-init --use-exwm 2>&1 | tee ~/debug.log
#xterm

# Fire it up
exec dbus-launch --exit-with-session emacs --debug-init

