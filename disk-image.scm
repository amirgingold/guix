(define-module (nongnu system install)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (nongnu packages linux)
  #:use-module (gnu packages certs) ;; nss-certs
  #:use-module (gnu packages disk) ;; sgdisk mkfs.fat
  #:use-module (guix gexp)
  #:export (installation-os-nonfree))

(define install-guix
  (program-file "install"
		#~(begin
		    (system* "wget" "https://github.com/amirgingold/guix/raw/main/install.sh")
		    (system* "sh" "install.sh"))))
  
(define installation-os-nonfree
  (operating-system
    (inherit installation-os)
    (kernel linux)
    (firmware (list linux-firmware))
    (packages
     (append (list nss-certs gptfdisk dosfstools) %base-packages))
    (setuid-programs
     (append (list install-guix) %setuid-programs))))

installation-os-nonfree
