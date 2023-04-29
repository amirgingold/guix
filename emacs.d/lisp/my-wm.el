
;; (load-file "~/.emacs.d/per-system-settings.el")

;;  (setup (:pkg exwm) ;; Should be uncommented when setup is defined

(setq mouse-autoselect-window nil
      focus-follows-mouse t)

(setq exwm-workspace-warp-cursor nil)
	  
;; Make class name the buffer name.
(add-hook 'exwm-update-class-hook
	  (lambda ()
	    (exwm-workspace-rename-buffer exwm-class-name)))
(add-hook 'exwm-update-title-hook
	  (lambda ()
	    (pcase exwm-class-name
	      ("Vimb" (exwm-workspace-rename-buffer (format "vimb: %s" exwm-title)))
	      ("qutebrowser" (exwm-workspace-rename-buffer (format "Qutebrowser: %s" exwm-title))))))

(exwm-enable)

;;    ) ;; Should be uncommented when setup is defined

;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;

;; Ctrl+q will enable the next key to be sent directly.
(define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

(setq exwm-input-global-keys
      `(
	;; Bind "s-&" to launch applications
	([?\s-&] . (lambda (command)
		     (interactive (list (read-shell-command "$ ")))
		     (start-process-shell-command command nil command)))))

;; Simulation keys to mimic the behavior of emacs.
(setq exwm-input-simulation-keys
      '(
	;; movement
	([?\C-b] . [left])
	([?\M-b] . [C-left])
	([?\C-f] . [right])
	([?\M-f] . [C-right])
	([?\C-p] . [up])
	([?\C-n] . [down])
	([?\C-a] . [home])
	([?\C-e] . [end])
	([?\M-v] . [prior])
	([?\C-v] . [next])
	([?\C-d] . [delete])
	([?\C-k] . [S-end delete])
	;; cut/paste
	([?\C-w] . [?\C-x])
	([?\M-w] . [?\C-c])
	([?\C-y] . [?\C-v])
	;; search
	([?\C-s] . [?\C-f])))

;;;;;;;;;;;;;;;;;
;; Input Methods
;;;;;;;;;;;;;;;;;

(setq default-input-method 'hebrew-new)
(setenv "GTK_IM_MODULE" "xim")
(setenv "QT_IM_MODULE" "xim")
(setenv "XMODIFIERS" "@im=exwm-xim")
(setenv "CLUTTER_IM_MODULE" "xim")
(require 'exwm-xim)
(exwm-xim-enable)
(push ?\C-\\ exwm-input-prefix-keys) ;; use Ctrl + \ to toggle between input methods


(provide 'my-wm)
