(use-modules (gnu)
	     (nongnu packages linux)
	     (nongnu system linux-initrd))
(use-service-modules desktop networking ssh xorg)

(operating-system
  (host-name "guix")
  (timezone "Asia/Jerusalem")
  (locale "en_US.utf8")
  
;; Use non-free Linux and firmware
  (kernel linux)
  (firmware (list linux-firmware))
  (initrd microcode-initrd)
  
  (bootloader
    (bootloader-configuration
      (bootloader grub-bootloader)
      (target "/dev/sda")))
  (file-systems
    (append (list
              (file-system
                (device (file-system-label "BOOT"))
                (mount-point "/boot")
                (type "vfat"))
              (file-system
                (device (file-system-label "ROOT"))
                (mount-point "/")
                (type "ext4")))
            %base-file-systems))
  (users (cons
           (user-account
             (name "me")
	     (password "")
             (group "users")
             (supplementary-groups '("wheel" "netdev" "audio" "video")))
           %base-user-accounts))
  (sudoers-file
    (plain-file "sudoers"
                (string-join '("root ALL=(ALL) ALL"
                               "%wheel ALL=NOPASSWD: ALL") "\n")))
  (packages (append (list
                      (specification->package "git")		     
                      (specification->package "emacs")
                      (specification->package "emacs-exwm")
                      (specification->package "emacs-desktop-environment")
                      (specification->package "nss-certs"))
                    %base-packages))
  (services %desktop-services))
