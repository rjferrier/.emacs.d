
;;; DIRED

(define-key dired-mode-map (kbd "H-s H-8") 'dired-do-isearch-regexp)


;;; SHELL & SHELL SCRIPT

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . shell-script-mode))

(define-key shell-mode-map (kbd "H-p")
  '(lambda () (interactive) (call-interactively 'comint-previous-input)))
(define-key shell-mode-map (kbd "H-P")
  '(lambda () (interactive) (call-interactively 'comint-next-input)))


