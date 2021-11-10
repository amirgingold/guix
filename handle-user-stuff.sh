#!/bin/sh

# Backup
ln -s /mnt/backup ~/backup

# Git
git config --global user.name amirgingold
git config --global user.email amirgingold@gmail.com
git config --global credential.helper store

# Emacs
git clone https://github.com/amirgingold/dotfiles.git ~/.dotfiles
mkdir ~/.config/emacs
ln -s ~/.dotfiles/emacs-init.el ~/.config/emacs/init.el
rm -r ~/.emacs.d

# Channels
mkdir ~/.config/guix
wget https://raw.githubusercontent.com/amirgingold/guix/main/channels.scm -P /home/me/.config/guix
guix pull

# Install packages
git clone https://github.com/amirgingold/guix.git
ls guix/packages/ | xargs -I tt guix package --install-from-file=guix/packages/tt
rm -r guix
