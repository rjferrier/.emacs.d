
;;; FIRST THINGS FIRST

;; add directories to the load path

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq user-lisp-directory (concat user-emacs-directory "/lisp/"))
(add-to-list 'load-path user-lisp-directory)
(let ((default-directory user-emacs-directory))
  (normal-top-level-add-subdirs-to-load-path))

;; load init-*.el files
(let ((file (directory-files user-lisp-directory t "^init-.*\.el$")))
  (while file
    (load-file (car file))
    (setq file (cdr file))))

;; set some system-invariant customisations...
(custom-set-variables
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(doc-view-continuous t t)
 '(inhibit-startup-screen t)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain))

;; themes
(add-to-list 'custom-theme-load-path
	     (concat user-emacs-directory "/themes/"))
(load-theme 'rjf t)

;; ... but allow the user to override these with system-dependent
;; customisations.  These will reside in a separate file and will not
;; be checked into the repository
(setq custom-file (concat user-emacs-directory "custom.el"))
(if (file-exists-p custom-file)
    (load custom-file)
  (write-region "" nil custom-file))

;; environment variables
(setq env-file (concat (substitute-in-file-name "$HOME")
		       "/.env.d/env.el"))
(when (file-exists-p env-file)
   (load env-file))
(setenv "PYTHONPATH" (substitute-in-file-name "$HOME"))

;; backup file settings
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;; Windows-specific
(defconst in-win32
  (or 
   (eq system-type 'cygwin)
   (eq system-type 'ms-dos)
   (eq system-type 'windows-nt)))
(when in-win32
  (message "Adjusting MS Windows key modifiers")
  (setq w32-alt-is-meta nil
	w32-pass-apps-to-system nil
	w32-pass-lwindow-to-system nil
	w32-pass-rwindow-to-system nil
	w32-lwindow-modifier 'meta
	w32-apps-modifier 'hyper)
  (w32-register-hot-key [H-<SPC>]))


;;; REPOSITORY MANAGEMENT

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

;;; BASIC ENVIRONMENT

;; get global functions and key bindings
(require 'global-keys)

;; prompt for the 'project directory' then dired into here
(add-hook 'after-init-hook
	  (lambda () (progn
	      (call-interactively 'set-project-dir)
	      (dired project-directory))))

(global-auto-revert-mode t)
(subword-mode t)
(ido-mode 'both)
(setq ido-enable-flex-matching t)

;; sequence and keybinding for expansions
(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs
	try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-file-name-partially
	try-complete-file-name
	try-expand-list
	try-expand-line
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol))



;;; WINDMOVE/FRAMEMOVE

(require 'framemove)
(setq framemove-hook-into-windmove t)


;;; SEMANTIC/WHICH FUNC

;; ;; warning: causes my laptop to hang...
;; (semantic-mode t)
;; (eval-after-load "semantic"
;;   (global-semantic-stickyfunc-mode 1))

;; put function info in the header area
(setq mode-line-format (delete (assoc 'which-func-mode
                                      mode-line-format) mode-line-format)
      which-func-header-line-format '(which-func-mode ("" which-func-format)))
(defadvice which-func-ff-hook (after header-line activate)
  (when which-func-mode
    (setq mode-line-format (delete (assoc 'which-func-mode
                                          mode-line-format) mode-line-format)
          header-line-format which-func-header-line-format)))


;;; COMPILATION AND DEBUGGING

(setq compilation-skip-threshold 2)
(setq-default compilation-read-command t)  
;; (defconst default-compile-command "make -w")

;; force compilation to take place in situ so that we can manipulate
;; the window it appears in.  Do not split if possible.
(add-to-list 'same-window-buffer-names "*compilation*")
(add-to-list 'same-window-buffer-names "*gud*")
(add-to-list 'same-window-regexps "\\*gud-\\([a-zA-Z0-9_.]*\\)\\*")
(setq pop-up-frames t)
(setq pop-up-frame-function 'next-frame)
(setq pop-up-windows nil)
(setq split-height-threshold nil)
(setq split-width-threshold nil)
(setq-default display-buffer-reuse-frames t)


;;; BUFFER LIST/DIRED ENHANCEMENTS

;; ; hide ephemeral buffers
;; (require 'ibuf-ext)
;; (add-to-list 'ibuffer-never-show-predicates "^\\*")

; replace standard buffer list with iBuffer
(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)

(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$\\|.+~$"))

(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-verbosity '())


;;; WHOLE-LINE-OR-REGION MODE

(require 'whole-line-or-region)
(whole-line-or-region-mode)

(defadvice whole-line-or-region-yank-mod
  (before whole-line-or-region-yank activate)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end) )))
(put 'dired-find-alternate-file 'disabled nil)


;;; BACK-BUTTON

(require 'back-button)
(back-button-mode 1)
