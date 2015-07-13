
;;; DIRED

(eval-after-load "dired"
    '(progn
       (define-key dired-mode-map (kbd "H-s H-8") 'dired-do-isearch-regexp)))


;;; SHELL & SHELL SCRIPT

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . shell-script-mode))


(eval-after-load "shell"
  '(progn
     (define-key shell-mode-map (kbd "H-p")
       '(lambda () (interactive) (call-interactively 'comint-previous-input)))
     (define-key shell-mode-map (kbd "H-P")
       '(lambda () (interactive) (call-interactively 'comint-next-input)))))


;; XML

(defun nxml-load-in-diamond ()
  (interactive)
  (start-process "diamond" "*Messages*" "diamond" (buffer-name)))
(global-set-key (kbd "A-c d") 'nxml-load-in-diamond)


;; DIFF

(eval-after-load "diff"
  '(progn
     (message "setting up diff bindings")
     (define-key diff-mode-map (kbd "a") 'diff-apply-hunk)
     (define-key diff-mode-map (kbd "b") 'diff-buffer-with-file)
     (define-key diff-mode-map (kbd "c") 'diff-next-complex-hunk)
     (define-key diff-mode-map (kbd "f") 'diff-tell-file-name)
     (define-key diff-mode-map (kbd "g") 'diff-goto-source)
     (define-key diff-mode-map (kbd "j") 'diff-kill-junk)
     (define-key diff-mode-map (kbd "k") 'diff-hunk-kill)
     (define-key diff-mode-map (kbd "l") 'diff-add-change-log-entries-other-window)
     (define-key diff-mode-map (kbd "m") 'diff-fixup-modifs)
     (define-key diff-mode-map (kbd "n") 'diff-hunk-next)
     (define-key diff-mode-map (kbd "p") 'diff-hunk-prev)
     (define-key diff-mode-map (kbd "r") 'diff-refine-hunk)
     (define-key diff-mode-map (kbd "s") 'diff-split-hunk)
     (define-key diff-mode-map (kbd "t") 'diff-test-hunk)
     (define-key diff-mode-map (kbd "u") 'diff-undo)
     (define-key diff-mode-map (kbd "v") 'diff-restrict-view)
     (define-key diff-mode-map (kbd "w") 'diff-delete-trailing-whitespace)
     (define-key diff-mode-map (kbd "B") 'diff-backup)
     (define-key diff-mode-map (kbd "K") 'diff-file-kill)
     (define-key diff-mode-map (kbd "N") 'diff-file-next)
     (define-key diff-mode-map (kbd "P") 'diff-file-prev)
     (define-key diff-mode-map (kbd "R") 'diff-reverse-direction)
     ))
