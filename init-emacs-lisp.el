
(defvar elisp-foo-body nil)

(defun elisp-define-foo ()
  "Defines or resets a test snippet called elisp-define-foo."
  (interactive)
  (message
   (if (use-region-p)
       (let ((str (buffer-substring-no-properties
		   (region-beginning) (region-end))))
	 (with-temp-buffer
	   (insert "(defun elisp-foo () (interactive) " str " )")
	   (eval-current-buffer)
	   (setq elisp-foo-body str))
	 "Stored this snippet for execution.")
     (progn
       (setq elisp-foo-body nil)
       "Reset snippet execution.")
     )))

(defun elisp-evaluate-dwim ()
  "If elisp-foo-body has been defined, evaluates elisp-foo and
prints the result if it is a string.  Otherwise, evaluates the current
region or buffer."
  (interactive)
  (let ((result
	 (if elisp-foo-body
	     (progn
	       (elisp-foo))
	   (if (use-region-p)
	       (let ((str (buffer-substring-no-properties
			   (region-beginning) (region-end))))
		 (with-temp-buffer
		   (insert str)
		   (eval-current-buffer)))
	     (progn
	       (eval-current-buffer)
	       "Evaluated current buffer")))))
    (when (stringp result)
      (message result))))


(eval-after-load "emacs-lisp"
  '(progn
     ;; local key bindings
     (define-key emacs-lisp-mode-map (kbd "<A-H-SPC>")
       'elisp-evaluate-dwim)
     (define-key emacs-lisp-mode-map (kbd "A-H-d <A-H-SPC>")
       'elisp-define-foo)
     ))


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
