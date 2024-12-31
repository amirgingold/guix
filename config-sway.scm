(define-module (daviwil systems base)
  #:use-module (srfi srfi-1)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system nss)
  #:use-module (gnu system setuid)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu packages video)
  #:use-module (nongnu system linux-initrd)
  #:export (system-config))

(use-service-modules guix admin sysctl pm nix avahi dbus cups desktop linux
                     mcron networking xorg ssh docker audio virtualization)

(use-package-modules audio video nfs certs shells ssh linux bash emacs gnome
                     networking wm fonts libusb cups freedesktop file-systems
                     version-control package-management vim)

(define-public os-config
  (operating-system
   (host-name "M")
   (timezone "Asia/Jerusalem")
   (locale "en_IL.utf8")
   (locale-definitions
    (list (locale-definition (source "en_US")
			     (name "en_US.UTF-8"))
	  (locale-definition (source "he_IL")
			     (name "he_IL.UTF-8"))))
   (keyboard-layout "us,il" #:options '("grp:alt_shift_toggle"))

   ;; Use non-free Linux and firmware
   (kernel linux)
   (firmware (list linux-firmware))
   (initrd microcode-initrd)

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
		 (name "me")
                 (group "users")
                 (home-directory "/home/me")
                 (supplementary-groups '("wheel"  ;; sudo
                                         "netdev" ;; network devices
                                         "kvm"
                                         "tty"
					 "seat"
                                         "input"
                                         "audio"    ;; control audio devices
                                         "video"))) ;; control video devices
                %base-user-accounts))
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

(define sway-config
  (map (lambda (str)
         (string-append str "\n"))
       (list
        "set $mod Mod4"))
   
   ;; Configure only the services necessary to run the system
   (services (append
              (modify-services %base-services
               (delete login-service-type)
               (delete mingetty-service-type)
               (delete console-font-service-type))
              (list
	       (service seatd-service-type)
	       (service greetd-service-type
			(greetd-configuration 
			 (greeter-supplementary-groups (list "video" "input" "seat"))
			 (terminals
			  (list
			   (greetd-terminal-configuration
			    (terminal-vt "1")
			    (terminal-switch #t)
			    (default-session-command
			     (greetd-wlgreet-sway-session
			      (sway-configuration sway-config))))
                           (greetd-terminal-configuration
			    (terminal-vt "2"))
			   (greetd-terminal-configuration
			    (terminal-vt "3"))
			   (greetd-terminal-configuration
			    (terminal-vt "4"))
			   (greetd-terminal-configuration
			    (terminal-vt "5"))
			   (greetd-terminal-configuration
			    (terminal-vt "6"))))))

               ;; Basic desktop system services (copied from %desktop-services)
               (service avahi-service-type)
               (service udisks-service-type)
               (service upower-service-type)
               (service cups-pk-helper-service-type)
               (service geoclue-service-type)
               (service polkit-service-type)
               (service dbus-root-service-type)
               fontconfig-file-system-service ;; Manage the fontconfig cache

               ;; Power and thermal management services
               (service thermald-service-type)
               (service tlp-service-type
                        (tlp-configuration
                         (cpu-boost-on-ac? #t)
                         (wifi-pwr-on-bat? #t)))

               ;; Sync system clock with time servers
               (service ntp-service-type)

   ;; Allow resolution of '.local' host names with mDNS
   (name-service-switch %mdns-host-lookup-nss)))

os-config




	     
