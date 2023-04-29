(defun my/plist-remove-properties (plist properties)
  (if (not plist)
      nil
    (append
     (if (not (member (car plist) properties))
         (list (car plist) (cadr plist)))
     (my/plist-remove-properties (nthcdr 2 plist) properties))))

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))

(provide 'my-helpers)
