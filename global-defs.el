;;; PERSONALISED DIRECTORIES

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


;;; OS-RELATED FUNCTIONS

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



;;; TEXT EDITING

(defun insert-chars (char-list)
  (let ((char char-list))
    (while char
      (insert-char (car char) 1)
      (setq char (cdr char)))))

(defvar fill-column-1 80)
(defvar fill-column-2 72)
(defun toggle-fill-column ()
  (interactive)
  (if (eq fill-column fill-column-1)
      (progn
	(setq fill-column fill-column-2)
	(message (format "fill-column set to %d" fill-column)))
    (progn
      (setq fill-column fill-column-1)
      (message (format "fill-column set to %d" fill-column)))))


;;; COMPILATION AND DEBUGGING

(setq compilation-skip-threshold 2)
(setq-default compilation-read-command t)  

(defvar console-window nil)
(defun set-console-window ()
  "Specifies a window for compilation and debugging"
  (interactive)
  (if console-window
      (progn
	(setq console-window nil)
	(message "Compilation/debugging window unset"))
    (progn
      (setq console-window (selected-window))
      ;; remember the current window configuration since gdb will
      ;; destroy it
      (set-register 0 (list (current-window-configuration) nil))
      (message "Compilation/debugging window set"))))

;; (defun select-console-window ()
;;   "Safely selects or sets the console window"
;;   (interactive)
;;   (condition-case nil
;;       (select-window console-window)
;;     (error (set-console-window))))

(defadvice next-error (before error-in-other-window activate)
  (when console-window
    (select-window console-window)
    (select-window (next-window))))


(defvar make-operand-pattern
  "^ *[^ ]*? +\\(?:-[^ ]*\\)* \\([A-Za-z0-9_-]*\\(?:\.[A-Za-z0-9]*\\)?\\)")

(defun wrapped-compile (dwim)
  (sit-for 0.1)
  (setq compilation-read-command (not dwim))
  (when console-window
    (select-window console-window))
  (cd project-directory)
  (call-interactively 'compile)
  (set-buffer "*compilation*")
  (string-match make-operand-pattern compile-command)
  (rename-buffer (format "*compilation-%s*" (match-string 1 compile-command))))

(defun compile-dwim ()
  (interactive)
  (wrapped-compile t))

(defun compile-with-query ()
  (interactive)
  (wrapped-compile nil))


(defun wrapped-debug ()
  (interactive)
  (when console-window
    (select-window console-window))
  (cd project-directory)
  (condition-case nil
      (call-interactively 'gud-gdb)
    (error nil))
  (insert "run"))


;;; CODE/ADVANCED NAVIGATION AND EDITING 

(setq-default level-1-regexp "")
(setq-default level-2-regexp "")
(setq-default level-3-regexp "")
(setq-default level-2-stride 1)
(setq-default level-3-stride 1)
(setq-default contents-level-regexp "")

(defvar navigate-lock nil)

(defun navigate-last-command ()
  (or (equal last-command 'navigate-up)
      (equal last-command 'navigate-down)
      (equal last-command 'navigate-left)
      (equal last-command 'navigate-right)
      (equal last-command 'navigate-further-up)
      (equal last-command 'navigate-further-down)
      (equal last-command 'navigate-up-toggle)
      (equal last-command 'navigate-down-toggle)
      (equal last-command 'navigate-left-toggle)
      (equal last-command 'navigate-right-toggle)
      (equal last-command 'navigate-further-up-toggle)
      (equal last-command 'navigate-further-down-toggle)))

(defun navigate-up-toggle ()
  (interactive)
  (setq navigate-lock (not navigate-lock))
  (navigate-up t))  
(defun navigate-up (&optional navigate-trigger)
  (interactive)
  (if (or (navigate-last-command)
	  navigate-trigger)
      (if (and navigate-lock
	       (> (length level-2-regexp) 0))
	  (loop repeat level-2-stride do
		(re-search-backward level-2-regexp nil t))
	(backward-paragraph))
    (progn
      (setq navigate-lock nil)
      (backward-paragraph))))

(defun navigate-down-toggle ()
  (interactive)
  (setq navigate-lock (not navigate-lock))
  (navigate-down t))
(defun navigate-down (&optional navigate-trigger)
  (interactive)
  (if (or (navigate-last-command)
	  navigate-trigger)
      (if (and navigate-lock
	       (> (length level-2-regexp) 0))
	  (loop repeat level-2-stride do
		  (re-search-forward level-2-regexp nil t))
	(forward-paragraph))
    (progn
      (setq navigate-lock nil)
      (forward-paragraph))))

(defun navigate-left-toggle ()
  (interactive)
  (setq navigate-lock (not navigate-lock))
  (navigate-left t))
(defun navigate-left (&optional navigate-trigger)
  (interactive)
  (if (or (navigate-last-command)
	  navigate-trigger)
      (if (and navigate-lock
	       (> (length level-3-regexp) 0))
	  (loop repeat level-3-stride do
		(re-search-backward level-3-regexp nil t))
	(subword-backward))
    (progn
      (setq navigate-lock nil)
      (subword-backward)))
  (setq navigate-trigger nil))

(defun navigate-right-toggle ()
  (interactive)
  (setq navigate-lock (not navigate-lock))
  (navigate-right t))
(defun navigate-right (&optional navigate-trigger)
  (interactive)
  (if (or (navigate-last-command)
	  navigate-trigger)
      (if (and navigate-lock
	       (> (length level-3-regexp) 0))
	  (loop repeat level-3-stride do
		(re-search-forward level-3-regexp nil t))
	(subword-forward))
    (progn
      (setq navigate-lock nil)
      (subword-forward))))

(defun navigate-further-up-toggle ()
  (interactive)
  (setq navigate-lock (not navigate-lock))
  (navigate-further-up t))  
(defun navigate-further-up (&optional navigate-trigger)
  (interactive)
  (if (or (navigate-last-command)
	  navigate-trigger)
      (if (and navigate-lock
	       (> (length level-1-regexp) 0))
	  (unless
	      (re-search-backward level-1-regexp nil t)
	    (goto-char (point-min)))
	(beginning-of-buffer))
    (progn
      (setq navigate-lock nil)
      (beginning-of-buffer))))

(defun navigate-further-down-toggle ()
  (interactive)
  (setq navigate-lock (not navigate-lock))
  (navigate-further-down t))
(defun navigate-further-down (&optional navigate-trigger)
  (interactive)
  (if (or (navigate-last-command)
	  navigate-trigger)
      (if (and navigate-lock
	       (> (length level-1-regexp) 0))
	  (unless
	      (re-search-forward level-1-regexp nil t)
	    (goto-char (point-max)))
	(end-of-buffer))
    (progn
      (setq navigate-lock nil)
      (end-of-buffer))))


(defun buffer-or-contents-name (return-contents-p)
  "If the input arg is non-nil and the name of the buffer is xyz or
*xyz-contents*, returns *xyz-contents*.  If the input arg is nil and the
name of the buffer is xyz or *xyz-contents*, returns xyz."
  (let (core-str)
    (if (string-match "\\*\\(.*?\\)-contents\\*" (buffer-name))
	(setq core-str (replace-match "\\1" nil nil (buffer-name)))
      (setq core-str (buffer-name)))
    (if return-contents-p
	(concat "*" core-str "-contents*")
      core-str)))


(add-hook 'occur-mode-hook (lambda () (setq truncate-lines t)))
(add-to-list 'same-window-regexps "-contents")
(add-to-list 'same-window-buffer-names "*Occur*")
(defun contents-pane (str-regexp)
  "Uses Occur to temporarily list subheadings for a buffer."
  (interactive)
  (let* ((cpn (buffer-or-contents-name t))
	 (cp (get-buffer-window cpn))
	 (hp (get-buffer-window
	      (buffer-or-contents-name nil))))
    ;; check whether an associated contents pane already exists
    (if cp
	(progn
	  ;; it does - do a clean up operation
	  (delete-window cp)
	  (kill-buffer cpn)
	  (select-window hp))
      (progn
	;; it does not - proceed with Occur operation, as long
	;; as the argument regexp is not empty
	(when (> (length str-regexp) 0)
	  ;; make a new sub-window and perform Occur with the
	  ;; supplied regexp argument.
	  (split-window-vertically)
	  ;; (add-to-list 'same-window-buffer-names cpn)
	  (occur str-regexp)
	  (let ((cpb (get-buffer "*Occur*")))
	    (if cpb
		(progn
		  ;; (select-window (get-buffer-window "*Occur*"))
		  ;; (rename-buffer cpn)
		  (set-buffer (get-buffer "*Occur*"))
		  (let ((n 0))
		    (save-excursion
		      (goto-char (point-min))
		      (setq n (string-to-number (thing-at-point 'word)))
		      (end-of-line) (forward-char 1)
		      (narrow-to-region (point) (point-max)))
		    (enlarge-window (- (min 15 (+ n 1)) (window-height))))
		  (rename-buffer cpn))
	      (delete-window))
	    ))))))

(defun contents ()
  (interactive)
  (contents-pane contents-level-regexp))

(defun last-element (str-regexp)
  (save-excursion
    (re-search-backward str-regexp))
  (message (match-string 1)))


;;; MISC

(defun copy-buffer-file-name (&optional with-path)
  (interactive)
  (let ((f (buffer-file-name)))
    (if f
	(progn
	  (unless with-path
	    (setq f (file-name-nondirectory f)))
	  (kill-new f))
      ;; if no file name, default to buffer name
      (kill-new (buffer-name)))))

(defun custom-time-stamp ()
  (when (region-active-p)
    (delete-region (region-beginning) (region-end) ))
  (format-time-string "%Y-%m-%dT%T"))

(defun shell-in-split-window ()
  (interactive)
  (split-window)
  (select-window (next-window))
  (shell))


;;; END
(provide 'global-defs)


