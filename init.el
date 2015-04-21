
;;; FIRST THINGS FIRST

;; add directories to the load path
(add-to-list 'load-path user-emacs-directory)
(let ((default-directory user-emacs-directory))
  (normal-top-level-add-subdirs-to-load-path))

;; load init-*.el files
(let ((file (directory-files user-emacs-directory t "^init-.*\.el$")))
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

;; ... but allow the user to override these with system-dependent
;; customisations.  These will reside in a separate file and will not
;; be checked into the repository
(setq custom-file (concat user-emacs-directory "custom.el"))
(if (file-exists-p custom-file)
    (load custom-file)
  (write-region "" nil custom-file))

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


;;; BASIC ENVIRONMENT

;; get global functions and key bindings
(require 'global-keys)

;; store the path Emacs was launched in as the 'project directory'
(defvar project-dir default-directory)
;; then prompt
(add-hook 'after-init-hook
	  (lambda () (call-interactively 'set-project-dir)))

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


;;; SEMANTIC

(semantic-mode t)
(eval-after-load "semantic"
  (global-semantic-stickyfunc-mode 1))

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

; can't remember what this does
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*")

; replace standard buffer list with iBuffer
(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)

(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$\\|.+~$"))


;;; WHOLE-LINE-OR-REGION MODE

(require 'whole-line-or-region)
(whole-line-or-region-mode)

(defadvice whole-line-or-region-yank-mod
  (before whole-line-or-region-yank activate)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end) )))
