# Updates
cd ~
mkdir .config/guix
cd .config/guix
wget https://github.com/amirgingold/guix/raw/main/channels.scm
cd ~
guix pull

git clone https://github.com/amirgingold/guix .guix
ln -s ~/.guix/xsession .xsession
ln -s ~/.guix/profile .profile
ln -s ~/.guix/emacs.d/init.el .emacs.d/init.el
ln -s /mnt/extern extern

# Git
git config --global user.name amirgingold
git config --global user.email amirgingold@gmail.com
git config --global credentials.helper store

# Regenerate personal access token
# When commiting to github, credentials are: username and the regenerated token

############
# Packages #
############

guix install ungoogled-chromium \
             emacs-zenburn-theme \
             gnome-icon-theme \
             xournalpp

# nixpkgs
nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
nix-channel --update



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
#   Settings -> Seach engine
#   Settings -> Autofill -> Passwords
#   Settings -> Security and Privacy -> Cookies and other site data -> Clear cookies and site data when you close all windows
