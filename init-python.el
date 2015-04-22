


(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "-i --pylab --profile=dev")


(eval-after-load "python"
  '(progn
     ;; source code width
     (set-fill-column 80)
     (setq python-fill-docstring-style 'symmetric)

     ;; local key bindings
     (define-key python-mode-map (kbd "<A-H-SPC>")
       'python-send-buffer)
     ))

(add-hook
 'python-mode-hook
 (lambda ()
   (progn
     ;; navigation
     (set (make-local-variable 'level-1-regexp)
	  "^### *\\([A-Za-z0-9]+.*\\)")
     (set (make-local-variable 'level-2-regexp)
	  "^\\(\\(?:class\\|def\\) +[a-zA-Z_].*\\)")
     (set (make-local-variable 'level-3-regexp)
	  "^\\(\\(?: *class +\\| *def +\\)?[a-zA-Z_].*\\)")
     (set (make-local-variable 'contents-level-regexp) level-2-regexp)
     )))
