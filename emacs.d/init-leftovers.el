;;;;;;;;;;;;;;;;;;;
;; System Settings
;;;;;;;;;;;;;;;;;;;

;; (load-file (expand-file-name "~/.guix/emacs.d/lisp/my-settings.el"))    

;; ;; Load settings for the first time                
;; (my/load-system-settings)                           



(setq my/profile-share-dir "~/.guix-profile/share/emacs/site-lisp/")

(defun my/add-to-path (load-path name)
  (let ((regexp (concat "^" name "-")))
    (add-to-list load-path
		 (car (directory-files-recursively my/profile-share-dir regexp t)))))

(defun my/add-pkg-to-path (name)
  (my/add-to-path 'load-path name))

(defun my/add-theme-to-path (name)
  (my/add-to-path 'custom-theme-load-path name))
  






;;;;;;;;;;;;;;;;;;;;
;; Update Load Path
;;;;;;;;;;;;;;;;;;;;

;; The library is added at the beginning
;; ;; Add my library path to load-path
;; (push (expand-file-name "~/.guix/emacs.d/lisp") load-path)

;; ;; Add packages locations
;; (let ((default-directory "/home/me/.guix-profilex/share/emacs/site-lisp/"))
;;   (normal-top-level-add-subdirs-to-load-path))


;; ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

;; ;;;;;;;;;;;;;;;;;;;
;; ;; Web Development
;; ;;;;;;;;;;;;;;;;;;;

;; ;; Set css indentation
;; (setq-default css-indent-offset 2)

;; ;; ;; Web Mode
;; ;; (setup (:pkg web-mode)
;; ;;   (my/add-pkg-to-path "web-mode")
;; ;;   (require 'web-mode)
;; ;;   (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

;; ;;(add-to-list 'auto-mode-alist '("\\.html?\\'" . html-mode))

;; ;; Helping js-mode with js2-mode and a dependency for skewer
;; (setup (:pkg js2-mode)
;;   (my/add-pkg-to-path "js2-mode")
;;   (require 'js2-mode))

;; ;; Dependency for live web development
;; (setup (:pkg simple-httpd)
;;   (my/add-pkg-to-path "simple-httpd")
;;   (require 'simple-httpd)
;;   (setq httpd-root "/home/me/httpd-root"))

;; ;; Filter
;; (setup (:pkg htmlize)
;;   (my/add-pkg-to-path "htmlize")
;;   (require 'htmlize))

;; ;; Had to be cloned from https://github.com/skeeto/impatient-mode
;; (add-to-list 'load-path "~/.emacs.d/impatient-mode")
;; (require 'impatient-mode)

;; ;; Making it prettier
;; (setup (:pkg prettier)
;;   (my/add-pkg-to-path "prettier")
;;   (require 'prettier-js)
;;   (add-hook 'html-mode-hook 'prettier-js-mode))

;; ;; Speeding up html writing
;; (setup (:pkg emmet-mode)
;;   (my/add-pkg-to-path "emmet-mode")
;;   (require 'emmet-mode)
;;   (add-hook 'html-mode-hook 'emmet-mode))


;; ;;;;;;;;;;
;; ;; Emojis
;; ;;;;;;;;;;

;;  (setup (:pkg dash)
;;    (my/add-pkg-to-path "dash")
;;    (require 'dash))

;; (setup (:pkg ht)
;;   (my/add-pkg-to-path "ht")
;;   (require 'ht))

;; (setup (:pkg emojify)
;;   (my/add-pkg-to-path "emojify")
;;   (require 'emojify))




;; ;;;;;;;;;
;; ;; Julia
;; ;;;;;;;;;

;; (setup (:pkg julia-mode)
;;   (my/add-pkg-to-path "julia-mode")
;;   (require 'julia-mode))

;; (setup (:pkg s) ;; julia-repl dependency
;;   (my/add-pkg-to-path "s")
;;   (require 's))

;; (setup (:pkg julia-repl)
;;   (my/add-pkg-to-path "julia-repl")
 ;;   (require 'julia-repl))

;; (add-hook 'julia-mode-hook 'julia-repl-mode) ;; always use minor mode

;; ;;;;;;;;;;;;;;;;;;
;; ;; C++ development
;; ;;;;;;;;;;;;;;;;;;

;; (setup (:pkg f) ;; lsp dependency
;;   (my/add-pkg-to-path "f")
;;   (require 'f))

;; (setup (:pkg lsp-mode)
;;   (my/add-pkg-to-path "lsp-mode")
;;   (require 'lsp-mode)
;;   (setq lsp-keymap-prefix "s-l")
;;   (add-hook 'c++-mode-hook #'lsp)
;;   (add-to-list 'load-path (expand-file-name "lib/lsp-mode" user-emacs-directory))
;;   (add-to-list 'load-path (expand-file-name "lib/lsp-mode/clients" user-emacs-directory)))

;; ;; (setup (:pkg yasnippet)
;; ;;   (my/add-pkg-to-path "yasnippet")
;; ;;   (require 'yasnippet)
;; ;;   (yas-global-mode 1))

;; ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

;;;;;;;;;;;;;;;;;
;; Shutting Down
;;;;;;;;;;;;;;;;;

(add-hook 'kill-emacs-hook (lambda () (shell-command "sudo shutdown") t))
