;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;
;; Startup Performance
;;;;;;;;;;;;;;;;;;;;;;;

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;;;;;;;;;;;;;;;;;;;
;; System Settings
;;;;;;;;;;;;;;;;;;;

(load-file (expand-file-name "~/.guix/emacs.d/lisp/my-settings.el"))

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

(require 'cl-lib)
(cl-flet ((filter-straight-recipe (recipe)
	    (let* ((plist (cdr recipe))
		   (name (plist-get plist :straight)))
	      (cons (if (and name (not (equal name t)))
			name
		      (car recipe))
		    (plist-put plist :straight nil)))))
  (setup-define :pkg
    (lambda (&rest recipe)
      (if (or (eq (length recipe) 1)
              (plist-get (cdr recipe) :guix))
          `(add-to-list 'my/guix-emacs-packages 
			,(or (plist-get recipe :guix)
                             (concat "emacs-" (symbol-name (car recipe)))))
        `(straight-use-package ',(filter-straight-recipe recipe))))
    :documentation "Install RECIPE via Guix or straight.el"
    :shorthand #'cadr))

;;;;;;;;;;;;;;;;;;;;;;;
;; Keep .emacs.d Clean
;;;;;;;;;;;;;;;;;;;;;;;

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(setup (:pkg no-littering))
(add-to-list 'load-path
	     (car (directory-files-recursively "~/.guix-profile/share/emacs/site-lisp/" "no-littering" t)))
(require 'no-littering)

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;;;;;;;;;;;;;;;;;;;;
;; Update Load Path
;;;;;;;;;;;;;;;;;;;;

;; Add my library path to load-path
(push (expand-file-name "~/.guix/emacs.d/lisp") load-path)

;; ;; Add packages locations
;; (let ((default-directory "/home/me/.guix-profile/share/emacs/site-lisp/"))
;;   (normal-top-level-add-subdirs-to-load-path))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Default Coding System
;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-default-coding-systems 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;
;; Desktop Environment
;;;;;;;;;;;;;;;;;;;;;;;

(require 'my-desktop)

;;;;;;;;;;;;;;;
;; Server Mode
;;;;;;;;;;;;;;;

(server-start)


;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Thanks, but no thanks
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

;;Improve scrolling.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX

;; Set frame transparency and maximize windows by default.

(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Enable line numbers and customize their format.

(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;Don’t warn for large files (shows up when launching videos)
(setq large-file-warning-threshold nil)

;;Don’t warn for following symlinked files
(setq vc-follow-symlinks t)

;;Don’t warn when advice is added for functions
(setq ad-redefinition-action 'accept)

;;;;;;;;;
;; Theme
;;;;;;;;;

(setup (:pkg zenburn-theme))
(add-to-list 'custom-theme-load-path
	     (car (directory-files-recursively "~/.guix-profile/share/emacs/site-lisp/" "zenburn-theme" t)))
(load-theme 'zenburn t)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tab Widths
(setq-default tab-width 2)

;; Use spaces instead of tabs for indentation
(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;
;; Shutting Down
;;;;;;;;;;;;;;;;;

(add-hook 'kill-emacs-hook (lambda () (shell-command "sudo shutdown") t))
