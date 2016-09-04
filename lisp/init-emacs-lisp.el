
(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (progn
     ;; navigation
     (set (make-local-variable 'level-1-regexp)
	  "^;;; *?\\([A-Za-z0-9]+.*\\)")
     (set (make-local-variable 'level-2-regexp) "^\\((def.*\\)")
     (set (make-local-variable 'level-3-regexp) "^ *\\((.*\\)")
     (set (make-local-variable 'contents-level-regexp) level-2-regexp)
     )))

