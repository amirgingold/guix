(define-module (nongnu system install)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (nongnu packages linux)
  #:use-module (gnu packages version-control) ;; git
  #:use-module (gnu packages certs) ;; nss-certs
  #:use-module (gnu packages disk) ;; sgdisk mkfs.fat
  #:use-module (guix gexp)
  #:export (installation-os-nonfree))

(define install
  (program-file "install"
		#~(begin
		    (system* "git" "clone" "https://github.com/amirgingold/guix")
		    (chdir "guix")
		    (system* "sh" "install.sh"))))
  
(define installation-os-nonfree
  (operating-system
    (inherit installation-os)
    (kernel linux)
    (firmware (list linux-firmware))
    (packages
     (append (list git nss-certs gptfdisk dosfstools) %base-packages))
    (setuid-programs
     (append (list install) %setuid-programs))))

installation-os-nonfree
