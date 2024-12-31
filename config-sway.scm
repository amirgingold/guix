;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(use-modules (gnu)
             (gnu system nss)
             (gnu home)
             (gnu home services)
             (gnu home services pm)
             (gnu home services gnupg)
             (gnu home services mcron)
             (gnu home services shells)
             (gnu home services desktop)
             (nongnu packages linux)
	     (nongnu system linux-initrd))

(use-service-modules desktop guix)
(use-package-modules bootloaders certs gnuzilla emacs emacs-xyz version-control wm
                     compression curl fonts freedesktop gimp glib gnome gnome-xyz
                     gstreamer kde-frameworks linux music package-management
                     password-utils pdf pulseaudio shellutils ssh syncthing video
                     web-browsers wget wm xdisorg xorg emacs)

(define user-name "me")

(define sway-config
  (map (lambda (str)
         (string-append str "\n"))
       (list
        "set $mod Mod4"
        "include \"~/.config/sway/before-config\""
        "bindsym $mod+space exec fuzzel -w 50 -x 8 -y 8 -r 3 -b 232635ff -t A6Accdff -s A6Accdff -S 232635ff -C c792eacc -m c792eacc -f \"JetBrains Mono:weight=light:size=10\" --no-fuzzy"
        "exec mako --border-radius=2 --font=\"JetBrains Mono 8\" --max-visible=5 --outer-margin=5 --margin=3 --background=\"#1c1f26\" --border-color=\"#89AAEB\" --border-size=1 --default-timeout=7000"
        "exec nm-applet --indicator"
        "exec emacs"
        "include \"~/.config/sway/after-config\"")))

;; Basic System Config
;; Home Config -- Configure Sway

(define home-config
  (home-environment
   (packages (list
              sway
              swaybg
              swayidle
              swaylock
              fuzzel
              mako
              grimshot
              network-manager-applet

              ;; Emacs
              emacs-pgtk

              ;; Browser
              icecat

              ;; Compatibility for older Xorg applications
              xorg-server-xwayland

              ;; Flatpak and XDG utilities
              xdg-utils ;; For xdg-open, etc
              xdg-dbus-proxy
              shared-mime-info
              (list glib "bin")

              ;; Appearance
              gnome-themes-extra
              adwaita-icon-theme

              ;; Fonts
              font-jetbrains-mono
              font-liberation

              ;; Audio utils
              alsa-utils
              pavucontrol

              ;; General utilities
              git
              curl
              wget
              openssh
              zip
              unzip
              trash-cli))

   (services (list
              (service home-xdg-configuration-files-service-type
                       `(("sway/config" ,(apply mixed-text-file (cons "sway-config" sway-config)))))))))

(define os-config
  (operating-system
   (host-name "M")

   (timezone "Asia/Jerusalem")
   (locale "en_US.utf8")

   (kernel linux)
   (firmware (list linux-firmware))
   (initrd microcode-initrd)

   (bootloader (bootloader-configuration
		(bootloader grub-efi-bootloader)
                (targets '("/boot"))
		(timeout 2)))
   
   (file-systems
    (append (list
	     (file-system
	      (device (file-system-label "BOOT"))
              (mount-point "/boot")
              (type "vfat"))
             (file-system
              (device (file-system-label "ROOT"))
              (mount-point "/")
              (type "ext4"))
	     (file-system
              (device (file-system-label "WORKSPACE"))
              (mount-point "/mnt/W")
              (type "ext4")))
	    %base-file-systems))

   (users (cons (user-account
		 (name user-name)
                 (group "users")
                 (supplementary-groups '("wheel" "netdev"
                                         "audio" "video")))
		%base-user-accounts))

    (services (append (list (service guix-home-service-type
                                     `((,user-name ,home-config))))
                      %desktop-services))

    ;; Allow resolution of '.local' host names with mDNS.
    (name-service-switch %mdns-host-lookup-nss)))

os-config
