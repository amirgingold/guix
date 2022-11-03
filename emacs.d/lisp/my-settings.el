(defun my/load-system-settings ()
  (interactive)
  (load-file "~/.guix/emacs.d/per-system-settings.el"))

(defun my/system-settings-get (setting)
  (alist-get setting my/system-settings))

(provide 'my-settings)
