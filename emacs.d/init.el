;;;;;;;;;;;;;;;;;;;;;;;
;; Startup Performance
;;;;;;;;;;;;;;;;;;;;;;;

;; Turn on lexical binding for the init file
;; -*- lexical-binding: t; -*-

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;;;;;;;;;;;;;;;;;;;
;; System Settings
;;;;;;;;;;;;;;;;;;;

(load-file (expand-file-name "~/.dotfiles/emacs.d/lisp/my-settings.el"))

;; Load settings for the first time
(my/load-system-settings)

;;;;;;;;;;;;;;;;;;;;;;
;; Package Management
;;;;;;;;;;;;;;;;;;;;;;

(unless (featurep 'straight)
  ;; Bootstrap straight.el
  (defvar bootstrap-version)
  (let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
       (unless (file-exists-p bootstrap-file)
         (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
       (load bootstrap-file nil 'nomessage)))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Streamlined Configuration with setup.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(straight-use-package '(setup :type git :host nil :repo "https://git.sr.ht/~pkal/setup"))
(require 'setup)

;;;;;;;;
;; :pkg
;;;;;;;;

;; Recipe is always a list
;; Install via Guix if length == 1 or :guix t is present

(defvar my/guix-emacs-packages '()
  "Contains a list of all Emacs package names that must be installed via Guix.")

;; Examples:
;; - (org-roam :straight t)
;; - (git-gutter :straight git-gutter-fringe)

(defun my/filter-straight-recipe (recipe)
  (let* ((plist (cdr recipe))
         (name (plist-get plist :straight)))
    (cons (if (and name (not (equal name t)))
              name
              (car recipe))
          (plist-put plist :straight nil))))

(setup-define :pkg
  (lambda (&rest recipe)
    (if (or (eq (length recipe) 1)
            (plist-get (cdr recipe) :guix))
        `(add-to-list 'my/guix-emacs-packages 
                      ,(or (plist-get recipe :guix)
                           (concat "emacs-" (symbol-name (car recipe)))))
        `(straight-use-package ',(my/filter-straight-recipe recipe))))
    :documentation "Install RECIPE via Guix or straight.el"
    :shorthand #'cadr)

;;;;;;;;;;;;;;;;;;;;
;; Update Load Path
;;;;;;;;;;;;;;;;;;;;

;; Add my library path to load-path
(push (expand-file-name "~/.guix/emacs.d/lisp") load-path)

;; Add packages locations
(let ((default-directory "/home/me/.guix-profile/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;;;;;;;;;;;;;;;;;;;;;;;
;; Keep .emacs.d Clean
;;;;;;;;;;;;;;;;;;;;;;;

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file (expand-file-name (format "emacs-custom.el") temporary-file-directory))
(load custom-file t)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Default Coding System
;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-default-coding-systems 'utf-8)

;;;;;;;;;;;;;;;
;; Server Mode
;;;;;;;;;;;;;;;

(server-start)

;;;;;;;;;;;;;;;;;;;;;;;
;; Desktop Environment
;;;;;;;;;;;;;;;;;;;;;;;

(require 'my-desktop)

(setup (:pkg zenburn-theme))
