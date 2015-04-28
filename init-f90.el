
(add-to-list 'auto-mode-alist '("[.]F90$" . f90-mode))
(setq f90-break-before-delimiters nil)
(setq f90-beginning-ampersand nil)


(eval-after-load "f90"
  '(progn
     ;; source code width
     (set-fill-column 80)
     ))

  
(add-hook
 'f90-mode-hook
 (lambda ()
   (progn
     ;; navigation
     (set (make-local-variable 'level-1-regexp)
	  "^ *\\(\\(?:end +\\)?module.*\\|contains\\)")
     (set (make-local-variable 'level-2-regexp)
	  "^ *\\(\\(subroutine\\|function\\|type\\) +[a-zA-Z_].*\\)")
     (set (make-local-variable 'level-3-regexp)
	  "^ *\\(\\(?:[a-zA-Z_]+: *\\)?\\(do\\|if\\|select\\).*\\)")
     (set (make-local-variable 'contents-level-regexp) level-2-regexp)
     )))
