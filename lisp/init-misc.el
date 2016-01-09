
;;; DIRED

(eval-after-load "dired"
    '(progn
       (define-key dired-mode-map (kbd "A-e A-7") 'dired-do-isearch-regexp)
       (define-key dired-mode-map (kbd "A-s A-i") 'dired-up-directory)
       (define-key dired-mode-map (kbd "A-s A-j") 'dired-prev-dirline)
       (define-key dired-mode-map (kbd "A-s A-l") 'dired-next-dirline)
       ))


;;; SHELL & SHELL SCRIPT

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . shell-script-mode))


(eval-after-load "shell"
  '(progn
     (define-key shell-mode-map (kbd "A-p")
       '(lambda () (interactive) (call-interactively 'comint-previous-input)))
     (define-key shell-mode-map (kbd "A-P")
       '(lambda () (interactive) (call-interactively 'comint-next-input)))))

(add-hook
 'eshell-mode-hook
 (lambda ()
   (progn
     (local-set-key (kbd "A-p")
       '(lambda () (interactive) (call-interactively 'eshell-previous-input)))
     (local-set-key (kbd "A-P")
       '(lambda () (interactive) (call-interactively 'eshell-next-input))))))


