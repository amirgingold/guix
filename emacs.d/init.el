;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;
;; Startup Performance
;;;;;;;;;;;;;;;;;;;;;;;

;;; Speed up init.
;;; Temporarily reduce garbage collection during startup. Inspect `gcs-done'.
(defun my/reset-gc-cons-threshold ()
  (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))))
(setq gc-cons-threshold (* 64 1024 1024))
(add-hook 'after-init-hook 'my/reset-gc-cons-threshold)


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Straight Installation
;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (load bootstrap-file nil 'nomessage))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Streamlined Configuration with setup.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(straight-use-package '(setup :type git :host nil :repo "https://git.sr.ht/~pkal/setup"))
(require 'setup)

;; Add my library path to load-path
(push (expand-file-name "~/.guix/emacs.d/lisp") load-path)

(require 'my-helpers)

(defvar my/guix-packages '()
  "Contains a list of all Emacs package names that must be installed via Guix.")

(setup-define :straight-pkg
  (lambda (&rest recipe)
    (let* ((pkg_name (car recipe))
           (plist (cdr recipe))
           (guix-dependencies (plist-get plist :guix-dependencies))
           (straight-recipe (cons pkg_name
                                  (my/plist-remove-properties plist
                                                              '(:guix-dependencies)))))
      `(progn
         (dolist (e ',guix-dependencies)
           (add-to-list 'my/guix-packages e))
         (straight-use-package ',straight-recipe))))
  :documentation "Install RECIPE via straight.el with optional Guix dependencies"
  :shorthand #'cadr)


;;;;;;;;;;;;;;;;;;;;;;;
;; Keep .emacs.d Clean
;;;;;;;;;;;;;;;;;;;;;;;

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(setup (:straight-pkg no-littering :type git :flavor melpa :host github
                      :repo "emacscollective/no-littering")
  (require 'no-littering))


(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))
;;(setq custom-file (expand-file-name "custom.el" temporary-file-directory))
(setq custom-file "/home/me/.guix/emacs.d/custom.el")
(load custom-file t)


;;;;;;;;;
;; Theme
;;;;;;;;;

(setup (:straight-pkg zenburn-theme :type git :flavor melpa :host github
                      :repo "bbatsov/zenburn-emacs")
  (load-theme 'zenburn t))


;;;;;;;;;;;;;
;; org-mode
;;;;;;;;;;;;;
(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (scheme . t)))

;;;;;;;;;
;; C/C++
;;;;;;;;;

(setup (:straight-pkg yasnippet :type git :flavor melpa :files ("yasnippet.el" "snippets" "yasnippet-pkg.el") :host github :repo "joaotavora/yasnippet"))

(setup (:straight-pkg lsp-treemacs :type git :flavor melpa :files (:defaults "icons" "lsp-treemacs-pkg.el") :host github :repo "emacs-lsp/lsp-treemacs"))

(setup (:straight-pkg helm-lsp :type git :flavor melpa :host github :repo "emacs-lsp/helm-lsp"))

(setup (:straight-pkg projectile :type git :flavor melpa :host github :repo "bbatsov/projectile"))

(setup (:straight-pkg hydra :type git :flavor melpa :files (:defaults (:exclude "lv.el") "hydra-pkg.el") :host github :repo "abo-abo/hydra"))

(setup (:straight-pkg flycheck :type git :flavor melpa :host github :repo "flycheck/flycheck"))

(setup (:straight-pkg company :type git :flavor melpa :files (:defaults "icons" ("images/small" "doc/images/small/*.png") "company-pkg.el") :host github :repo "company-mode/company-mode"))

(setup (:straight-pkg avy :type git :flavor melpa :host github :repo "abo-abo/avy"))

(setup (:straight-pkg which-key :type git :flavor melpa :host github :repo "justbur/emacs-which-key"))

(setup (:straight-pkg helm-xref :type git :flavor melpa :host github :repo "brotzeit/helm-xref"))

(setup (:straight-pkg dap-mode :type git :flavor melpa :files (:defaults "icons" "dap-mode-pkg.el") :host github :repo "emacs-lsp/dap-mode"))

(setup (:straight-pkg lsp-mode :type git :flavor melpa :files (:defaults "clients/*.el" "lsp-mode-pkg.el") :host github :repo "emacs-lsp/lsp-mode" :guix-dependencies ("clang" "bear"))
  (helm-mode)
  (require 'helm-xref)
  (define-key global-map [remap find-file] #'helm-find-files)
  (define-key global-map [remap execute-extended-command] #'helm-M-x)
  (define-key global-map [remap switch-to-buffer] #'helm-mini)

  (which-key-mode)
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)

  (setq gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024)
        treemacs-space-between-root-nodes nil
        company-idle-delay 0.0
        company-minimum-prefix-length 1
        lsp-idle-delay 0.1)  ;; clangd is fast

  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
    (require 'dap-cpptools)
    (yas-global-mode)))


;;;;;;;;;;;
;; geiser
;;;;;;;;;;;

(setup (:straight-pkg ac-geiser :type git :flavor melpa :host github :repo "xiaohanyu/ac-geiser"))

;;;;;;;;;;;;;;;;;
;; geiser-guile
;;;;;;;;;;;;;;;;;

(setup (:straight-pkg geiser-guile :type git :flavor melpa :files (:defaults ("src" "src/*") "geiser-guile-pkg.el") :host gitlab :repo "emacs-geiser/guile"))


;;;;;;;;;
;; Latex
;;;;;;;;;

(setup (:straight-pkg auctex :type git :host github
                      :files ("*" (:exclude ".git")) :repo "emacs-straight/auctex"
                      :guix-dependencies ("texlive" "ghostscript" "make" "texinfo" "evince" "font-culmus"))
  (load "auctex.el" nil t t)
  
  ;; Getting support for many of the LaTeX packages
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)

  (setq-default TeX-engine 'xetex)
  (setq-default TeX-PDF-mode t)

  ;;  (setq-default TeX-master nil)
  (setq TeX-command-force "LaTeX")   
  (setq TeX-view-evince-keep-focus t)
  (setq TeX-clean-confirm t)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Default Coding System
;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-default-coding-systems 'utf-8)


;;;;;;;;;;;;;;;
;; Server Mode
;;;;;;;;;;;;;;;

(server-start)

(require 'my-wm)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;


;; Set sources directory
(setq find-function-C-source-directory (concat "~/src/emacs-" emacs-version "/src"))
;;(setq source-directory "/home/me/emacs-28.2")

;; Thanks, but no thanks
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; No need for lockfiles
(setq create-lockfiles nil)

(scroll-bar-mode -1)                  ; Disable visible scrollbar
(tool-bar-mode -1)                    ; Disable the toolbar
(tooltip-mode -1)                     ; Disable tooltips
(set-fringe-mode 10)                  ; Give some breathing room
;;(setq show-trailing-whitespace t)     ; Show trailing whitespaces

;; Show time in mode-line
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

;; Disable emacs automatic backup
(setq make-backup-files nil)

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

;;Improve scrolling.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-conservatively 100) ;; keyboard scroll one line at a time. 100 is a big enough number for something
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

;; Write Guix packages names to manifest
(write-region (concat
               "(specifications->manifest '"
               (format "%s" my/guix-packages)
               ")")
              nil
              "~/.guix/manifest-dependencies.scm")
