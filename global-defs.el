;; PERSONALISED DIRECTORIES

;; store the path Emacs was launched in as the 'project directory'
(defvar project-directory default-directory)

(defun set-project-dir (str)
  (interactive (list (ido-read-directory-name
		      "Set project directory: "
		      default-directory)))
  (setq project-directory str)
  (cd str)
  (when (file-exists-p (concat project-directory "TAGS"))
    (visit-tags-table (concat project-directory "TAGS"))))

(defun dired-user-emacs-directory ()
  (interactive)
  (dired user-emacs-directory))


;; OS-RELATED FUNCTIONS

(defun launch-nautilus-here ()
  (interactive)
  (shell-command "nautilus $PWD"))

(defun launch-terminal-here ()
  (interactive)
  (shell-command "gnome-terminal $PWD"))


;;; BASIC FUNCTIONS

(defun wrapped-revert-buffer ()
  "Swift revert buffer command"
  (interactive)
  (revert-buffer nil t))

(defun kill-process-buffer (buffer)
  (let* ((doomed-process (get-buffer-process buffer)))
    (when doomed-process
      (delete-process doomed-process))
    (when buffer
      (kill-buffer buffer))))

(defun save-and-kill-buffer ()
  "Save-and-close"
  (interactive)
    (if (buffer-file-name)
      (save-buffer)  ;; save if this buffer is associated with a file
    (setq set-buffer-modified-p nil)) ;; otherwise, allow swift discard
  (kill-process-buffer (current-buffer)))

(defun wrapped-write-file (str)
  "For overriding the equivalent IDO op."
  (interactive "sWrite file (to default directory): ")
  (write-file (concat default-directory str)))

(defun wrapped-kill-buffer ()
  "For overriding the equivalent IDO op."
  (interactive)
  (kill-buffer))

(defun wrapped-find-file (str)
  "For overriding the equivalent IDO op."
  (interactive "sFind file (in default directory): ")
  (find-file (concat default-directory str)))

(defun save-and-kill-buffer-and-window ()
  "Useful for pop-ups."
  (interactive)
  (if (buffer-file-name)
      (save-buffer)
    (setq set-buffer-modified-p nil))
  (kill-process-buffer (current-buffer))
  (delete-window))



;;; TEXT SELECTION

(defun previous-line-with-mark ()
  (interactive)
  (cua-set-mark)
  (previous-line))
(defun backward-char-with-mark ()
  (interactive)
  (cua-set-mark)
  (backward-char))
(defun next-line-with-mark ()
  (interactive)
  (cua-set-mark)
  (next-line))
(defun forward-char-with-mark ()
  (interactive)
  (cua-set-mark)
  (forward-char))
(defun beginning-of-line-with-mark ()
  (interactive)
  (cua-set-mark)
  (move-beginning-of-line nil))
(defun end-of-line-with-mark ()
  (interactive)
  (cua-set-mark)
  (move-end-of-line nil))

(defun backward-paragraph-with-mark ()
  (interactive)
  (cua-set-mark)
  (backward-paragraph))
(defun backward-word-with-mark ()
  (interactive)
  (cua-set-mark)
  (subword-backward))
(defun forward-paragraph-with-mark ()
  (interactive)
  (cua-set-mark)
  (forward-paragraph))
(defun forward-word-with-mark ()
  (interactive)
  (cua-set-mark)
  (subword-forward))
(defun beginning-of-buffer-with-mark ()
  (interactive)
  (cua-set-mark)
  (beginning-of-buffer))
(defun end-of-buffer-with-mark ()
  (interactive)
  (cua-set-mark)
  (end-of-buffer))


;;; EMACS LISP

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


;;; COMPILATION AND DEBUGGING

(setq compilation-skip-threshold 2)
(setq-default compilation-read-command t)  

(defvar console-window (selected-window))
(defun set-console-window ()
  "Specifies a window for compilation and debugging"
  (interactive)
  (setq console-window (selected-window))
  ;; remember the current window configuration since gdb will destroy it
  (set-register 0 (list (current-window-configuration) nil))
  (message "Console window and frame layout set"))

(defun select-console-window ()
  "Safely selects or sets the console window"
  (interactive)
  (condition-case nil
      (select-window console-window)
    (error (set-console-window))))

(defadvice next-error (before error-in-other-window activate)
  (select-window console-window)
  (select-window (next-window)))

(defun wrapped-compile (dwim)
  (sit-for 0.1)
  (setq compilation-read-command (not dwim))
  (select-console-window)
  (cd project-directory)
  (call-interactively 'compile)
  (sit-for 1))

(defun compile-dwim ()
  (interactive)
  (wrapped-compile t))

(defun compile-with-query ()
  (interactive)
  (wrapped-compile nil))


;;; MISC

(defun shell-in-split-window ()
  (interactive)
  (split-window)
  (select-window (next-window))
  (shell))


;;; END
(provide 'global-defs)
