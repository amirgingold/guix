(require 'map) ;; Needed for map-merge

(setq dw/system-settings
  (map-merge
    'list
    '((desktop/dpi . 180)
;;      (desktop/background . "samuel-ferrara-uOi3lg8fGl4-unsplash.jpg")
      (emacs/default-face-size . 220)
      (emacs/variable-face-size . 245)
      (emacs/fixed-face-size . 200))))
