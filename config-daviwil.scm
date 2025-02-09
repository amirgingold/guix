(use-modules (gnu)
             (gnu system nss)
             (gnu home)
             (gnu home services)
             (gnu home services pm)
             (gnu home services gnupg)
             (gnu home services mcron)
             (gnu home services shells)
             (gnu home services desktop)
             (gnu system locale)
             (nongnu packages linux))

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
        "bindsym $mod+space exec fuzzel -w 50 -x 8 -y 8 -r 3 -b 232635ff -t A6Accdff -s A6Accdff -S 232635ff -C c792eacc -m c792eacc --no-fuzzy"
        "exec mako --border-radius=2 --max-visible=5 --outer-margin=5 --margin=3 --background=\"#1c1f26\" --border-color=\"#89AAEB\" --border-size=1 --default-timeout=7000"
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
   
    (locale "en_IL.utf8")
    (locale-definitions
     (list (locale-definition (source "en_US")
                              (name "en_US.UTF-8"))
           (locale-definition (source "he_IL")
                              (name "he_IL.UTF-8"))))
   (keyboard-layout (keyboard-layout "us,il" #:options '("grp:shifts_toggle" "grp_led:num")))
   
   (kernel linux)
   (firmware (list linux-firmware))

   (bootloader
    (bootloader-configuration
    (bootloader grub-efi-bootloader)
    (targets '("/boot"))
    (keyboard-layout keyboard-layout)
    (timeout 3)))
   
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

   (services (append
              (modify-services %base-services
                               (delete login-service-type)
		                          (delete mingetty-service-type)
		                          (delete console-font-service-type))
              (list
               (service greetd-service-type
                        (greetd-configuration
                         (greeter-supplementary-groups (list "video" "input"))
                         (terminals
                          (list
                           ;; TTY1 is the graphical login screen for Sway
                           (greetd-terminal-configuration
                            (terminal-vt "1")
                            (terminal-switch #t)
                            ;; (default-session-command (greetd-wlgreet-sway-session
                            ;;                           (sway-configuration
                            ;;                            (plain-file "sway-greet.conf"
                            ;;                                        "output * bg /home/daviwil/.dotfiles/backgrounds/samuel-ferrara-uOi3lg8fGl4-unsplash.jpg fill\n"))))
                            ))))))))

	       ;; Set up remaining TTYs for terminal use
	       (greetd-terminal-configuration (terminal-vt "2"))
	       (greetd-terminal-configuration (terminal-vt "3"))))))



   


   
   ;; Allow resolution of '.local' host names with mDNS.
   (name-service-switch %mdns-host-lookup-nss)))

os-config
