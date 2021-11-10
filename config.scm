(use-modules (gnu))
(use-service-modules desktop networking ssh xorg)

(define handle-root-stuff
  (program-file "handle-root-stuff"
		#~(begin
		    (let ((me (getpwnam "me")))
		      (chown "/mnt/backup" (passwd:uid me) (passwd:gid me))))))

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
    (bootloader grub-efi-bootloader)
    (target "/boot")))
  (file-systems
    (append (list
              (file-system
                (device (file-system-label "my-boot"))
                (mount-point "/boot")
                (type "vfat"))
              (file-system
                (device (file-system-label "my-root"))
                (mount-point "/")
                (type "ext4"))
              (file-system
                (device (file-system-label "backup"))
                (mount-point "/mnt/backup")
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
  (setuid-programs (cons handle-root-stuff %setuid-programs))
  (services %desktop-services))
