# Pulling updates
cd ~
mkdir .config/guix
cd .config/guix
wget https://github.com/amirgingold/guix/raw/main/channels.scm
cd ~
guix pull

# Chromium
guix install ungoogled-chromium
# Extensions
#   Go to https://github.com/NeverDecaf/chromium-web-store and do what's on the readme file.
#   Go to the chrome web store and install:
#     Chrome Remote Desktop
#     Ublock Origin
# Settings -> Seach engine
# Settings -> Autofill -> Passwords

# Flatpak apps
guix install flatpak
flatpak --user remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo

# Zoom
flatpak install --user --assumeyes --noninteractive flathub us.zoom.Zoom

# Skype
flatpak install --user --assumeyes --noninteractive flathub com.skype.Client
