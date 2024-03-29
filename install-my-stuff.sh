# Configuring Git
git config --global user.name amirgingold
git config --global user.email amirgingold@gmail.com
git config --global credential.helper store

# Regenerate personal access token
# When commiting to github, credentials are: username and the regenerated token

# Cloning my repo
git clone https://github.com/amirgingold/guix .guix

# Symlinking dotfiles
ln -s ~/.guix/xsession .xsession
ln -s ~/.guix/profile .profile
ln -s ~/.guix/emacs.d/init.el .emacs.d/init.el
ln -s ~/.guix/emacs.d/early-init.el .emacs.d/early-init.el

# Symlinking my external drive
ln -s /mnt/extern extern

# Symlinking channels and pulling
mkdir .config/guix
ln -s ~/.guix/channels.scm ~/.config/guix/channels.scm
guix pull

# Downloading emacs sources
mkdir src
cd src
emacs_version="$(emacs --version | grep -oE '^GNU Emacs [[:digit:]]{1,}(\.[[:digit:]]{1,})*' | cut -c11-)"
emacs_file=emacs-$emacs_version.tar.xz
wget mirror.rabisu.com/gnu/emacs/$emacs_file
tar -axvf $emacs_file
rm $emacs_file
cd ..

# Setting libvirt networking --weird it's not working--
sudo mkdir -p /usr/share/libvirt/networks
sudo cp ~/.guix/networking/default.xml /usr/share/libvirt/networks
sudo virsh net-define /usr/share/libvirt/networks/default.xml
sudo virsh net-autostart default
sudo virsh net-start default

# Installing dependencies
guix package --manifest=~/.guix/manifest-dependencies.scm

# Installing packages
guix package --manifest=~/.guix/manifest-packages.scm



############
# Packages #
############

# guix install \
#      emacs-zenburn-theme \
#      emacs-no-littering \
#      alsa-utils \
#      gnome-icon-theme \
#      xournalpp

# If nix is needed, then uncomment the region
# # nixpkgs
# nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
# nix-channel --update
# # nix packages
# nix-env -i firefox



# xourallpp depends on gnome-icon-theme

# If texlive is wanted then all 4 are needed
#	     texlive \
#             ghostscript \
#             make \
#             texinfo

############
# Settings #-chromium
#   Extensions
#     Go to https://github.com/NeverDecaf/chromium-web-store and do what's on the readme file.
#     Go to the chrome web store and install:
#       Chrome Remote Desktop
#       Ublock Origin
#   Settings -> Search engine
#   Settings -> Autofill -> Passwords: Toggle Offer to save passwords
#   Settings -> Privacy and Security -> Cookies and other site data: Toggle Clear cookies and site data when you close all windows

#############
# EPSON PERFECTION V500
#  Go to https://download.ebz.epson.net/dsc/search/01/search/searchModule and download the relevant driver.
#  Extract the .gz file, cd into the .deb directory and run ./install.sh
#  Install libcanberra-gtk-module to annihilate a warning: sudo apt-get install libcanberra-gtk-module
