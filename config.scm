(use-modules (gnu)
             (gnu system locale)
    	       (nongnu packages linux)
	           (nongnu system linux-initrd))
(use-service-modules
 desktop networking ssh xorg virtualization)
;; (use-service-modules nix)

(operating-system
 (host-name "guix")
 (timezone "Asia/Jerusalem")
 (locale "en_IL.utf8")
 (locale-definitions
  (list (locale-definition (source "en_US")
                           (name "en_US.UTF-8"))
        (locale-definition (source "he_IL")
                           (name "he_IL.UTF-8"))))

 (kernel linux)
 (firmware (list linux-firmware))
 (initrd microcode-initrd)

 (keyboard-layout (keyboard-layout "us,il" #:options '("grp:shifts_toggle" "grp_led:num")))
 
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
            (device (file-system-label "EXTERN"))
            (mount-point "/mnt/extern")
            (type "ext4")))
          %base-file-systems))
 
 (users (cons
         (user-account
          (name "me")
	        (password "")
          (group "users")
          (supplementary-groups '("wheel" "netdev" "audio" "video"
				                          "kvm" "libvirt")))
         %base-user-accounts))
 
 (sudoers-file
  (plain-file "sudoers"
              (string-join '("root ALL=(ALL) ALL"
                             "%wheel ALL=NOPASSWD: ALL") "\n")))
 
 (packages (append (list
                    (specification->package "emacs")
                    (specification->package "emacs-exwm")
                    (specification->package "emacs-desktop-environment")
                    (specification->package "nss-certs")
                    (specification->package "git")
;;		                (specification->package "nix")
		                (specification->package "xhost")
                    (specification->package "virt-manager"))
                   %base-packages))
 
 (services (cons* (service libvirt-service-type
                           (libvirt-configuration
                            (unix-sock-group "libvirt")))
                  (service virtlog-service-type)
;;            		  (service nix-service-type)
            		  (service slim-service-type
          			           (slim-configuration
                            (auto-login? #t)
                            (default-user "me")
                            (xorg-configuration
                                  (xorg-configuration
                                    (keyboard-layout keyboard-layout)))))
		              (modify-services %desktop-services
 				                           (delete gdm-service-type)))))
