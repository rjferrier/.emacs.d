
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "-i --pylab --profile=dev")


(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "<A-H-SPC>")
       'python-send-buffer)
     (define-key python-mode-map (kbd "<A-SPC>")
       'compile-dwim)
     ))

(add-hook
 'python-mode-hook
 (lambda ()
   (progn
     ;; source code width
     (set-fill-column 70)
     (setq python-fill-docstring-style 'symmetric)

     (define-key python-mode-map (kbd "<A-H-SPC>")
       'python-send-buffer)
     (define-key python-mode-map (kbd "<A-SPC>")
       'compile-dwim)
     )))
