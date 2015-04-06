;; add directories to the load path
(add-to-list 'load-path user-emacs-directory)
(let ((default-directory user-emacs-directory))
  (normal-top-level-add-subdirs-to-load-path))

;; set up environment
(subword-mode t)
(ido-mode 'both)
(setq ido-enable-flex-matching t)

;; set up compilation/debugging
(setq compilation-skip-threshold 2)
(setq-default compilation-read-command t)  

;; get functions and key bindings
(require 'global-keys)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(doc-view-continuous t t)
 '(inhibit-startup-screen t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(font-latex-sectioning-5-face ((((class color) (background dark)) (:inherit variable-pitch :foreground "white" :weight bold))))
 '(font-lock-keyword-face ((((class color) (min-colors 88) (background dark)) (:foreground "PaleVioletRed1"))))
 '(region ((t (:background "gray40" :foreground "white")))))
