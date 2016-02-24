(deftheme rjf
  "Simple dark theme")

(custom-theme-set-variables
 'rjf
 '(cua-mode t)
 '(doc-view-continuous t)
 '(inhibit-startup-screen t)
 '(column-number-mode t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-theme-set-faces
 'rjf
 '(default ((t (:background "black" :foreground "white"))))
 '(font-lock-comment-face ((t (:foreground "chocolate1" :slant italic))))
 '(region ((t (:background "dim gray")))))

(provide-theme 'rjf)
