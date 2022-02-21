(use-modules (gnu)
    	     (nongnu packages linux)
	     (nongnu system linux-initrd))
(use-service-modules desktop networking ssh xorg)
(use-service-modules virtualization)

(operating-system
  (host-name "guix")
  (timezone "Asia/Jerusalem")
  (locale "en_US.utf8")
  
  (kernel linux)
  (firmware (list linux-firmware))
  (initrd microcode-initrd)
  
  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets '("/boot"))))
  
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
                      (specification->package "virt-manager"))
                    %base-packages))
  
  (services
    (cons* (service libvirt-service-type
                    (libvirt-configuration
                      (unix-sock-group "libvirt")))
           (service virtlog-service-type)
           %desktop-services)))
