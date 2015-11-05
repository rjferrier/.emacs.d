
;;; DIRED

(eval-after-load "dired"
    '(progn
       (define-key dired-mode-map (kbd "A-c A-7") 'dired-do-isearch-regexp)))


;;; SHELL & SHELL SCRIPT

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . shell-script-mode))


(eval-after-load "shell"
  '(progn
     (define-key shell-mode-map (kbd "A-p")
       '(lambda () (interactive) (call-interactively 'comint-previous-input)))
     (define-key shell-mode-map (kbd "A-P")
       '(lambda () (interactive) (call-interactively 'comint-next-input)))))


