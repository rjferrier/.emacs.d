(require 'global-defs)
(require 'whole-line-or-region)
(require 'wrapped-windmove)
(require 'wrapped-highlight-symbol)


;;; text navigation and editing

;; basic functions
(define-key global-map (kbd "A-i") 'previous-line)
(define-key global-map (kbd "A-j") 'backward-char)
(define-key global-map (kbd "A-k") 'next-line)
(define-key global-map (kbd "A-l") 'forward-char)
(define-key global-map (kbd "A-u") 'subword-backward)
(define-key global-map (kbd "A-o") 'subword-forward)
(define-key global-map (kbd "A-y") 'backward-delete-char-untabify)
(define-key global-map (kbd "A-h") 'delete-char)
(define-key global-map (kbd "A-n") 'reindent-then-newline-and-indent)
(define-key global-map (kbd "A-.") 'whole-line-or-region-kill-region)
(define-key global-map (kbd "A-,") 'whole-line-or-region-kill-ring-save)
(define-key global-map (kbd "A-m") 'whole-line-or-region-yank)
(define-key global-map (kbd "A-M") 'yank-pop)

(define-key global-map (kbd "A-d A-i") 'beginning-of-buffer)
(define-key global-map (kbd "A-d A-j") 'move-beginning-of-line)
(define-key global-map (kbd "A-d A-l") 'move-end-of-line)
(define-key global-map (kbd "A-d A-k") 'end-of-buffer)
(define-key global-map (kbd "A-d A-u") 'backward-paragraph)
(define-key global-map (kbd "A-d A-o") 'forward-paragraph)
(define-key global-map (kbd "A-d A-y") 'subword-backward-kill)
(define-key global-map (kbd "A-d A-h") 'subword-kill)
(define-key global-map (kbd "A-d A-l") 'move-end-of-line)
(define-key global-map (kbd "A-d A-n") 'fill-paragraph)

(define-key global-map (kbd "A-s A-;") 'comment-dwim)
(define-key global-map (kbd "A-s ;") 'comment-set-column)

;; select text
(define-key global-map (kbd "A-I") 'previous-line-with-mark)
(define-key global-map (kbd "A-J") 'backward-char-with-mark)
(define-key global-map (kbd "A-K") 'next-line-with-mark)
(define-key global-map (kbd "A-L") 'forward-char-with-mark)
(define-key global-map (kbd "A-U") 'backward-word-with-mark)
(define-key global-map (kbd "A-O") 'forward-word-with-mark)

(define-key global-map (kbd "A-d A-I") 'beginning-of-buffer-with-mark)
(define-key global-map (kbd "A-d A-J") 'beginning-of-line-with-mark)
(define-key global-map (kbd "A-d A-K") 'end-of-buffer-with-mark)
(define-key global-map (kbd "A-d A-L") 'end-of-line-with-mark)
(define-key global-map (kbd "A-d A-U") 'backward-paragraph-with-mark)
(define-key global-map (kbd "A-d A-O") 'forward-paragraph-with-mark)

;; (convenience)
(define-key global-map (kbd "A-D A-I") 'beginning-of-buffer-with-mark)
(define-key global-map (kbd "A-D A-J") 'beginning-of-line-with-mark)
(define-key global-map (kbd "A-D A-K") 'end-of-buffer-with-mark)
(define-key global-map (kbd "A-D A-L") 'end-of-line-with-mark)
(define-key global-map (kbd "A-D A-U") 'backward-paragraph-with-mark)
(define-key global-map (kbd "A-D A-O") 'forward-paragraph-with-mark)


;; search and replace
(define-key global-map (kbd "A-7") 'isearch-forward-regexp)
(define-key global-map (kbd "A-&") 'isearch-backward-regexp)
(define-key global-map (kbd "A-8") 'wrapped-next-symbol)
(define-key global-map (kbd "A-*") 'wrapped-prev-symbol)
(add-hook
 'isearch-mode-hook
 (lambda ()
   (define-key isearch-mode-map (kbd "A-7") 'isearch-repeat-forward))
   (define-key isearch-mode-map (kbd "A-&") 'isearch-repeat-backward))
(define-key global-map (kbd "A-d A-7") 'highlight-symbol-at-point)

(define-key global-map (kbd "A-d 7") 'query-replace-regexp)
(define-key global-map (kbd "A-d 8") 'highlight-symbol-query-replace)

;; tags
(define-key global-map (kbd "A-s <f7>") 'create-tags)
(define-key global-map (kbd "A-s 7") 'find-tag)
(define-key global-map (kbd "A-9")
  (lambda () (interactive) (find-tag (pop-tag-mark) t)))
(define-key global-map (kbd "A-(") 'pop-tag-mark)


;; useful messages
(define-key global-map (kbd "A-s f")
  (lambda () (interactive) (insert (buffer-file-name))))
(define-key global-map (kbd "A-s t")
  (lambda () (interactive) (insert (custom-time-stamp))))
(define-key global-map (kbd "A-s 0")
  (lambda () (interactive) (message (last-element contents-level-regexp))))
(define-key global-map (kbd "A-s 1")
  (lambda () (interactive) (message (last-element level-1-regexp))))
(define-key global-map (kbd "A-s 2")
  (lambda () (interactive) (message (last-element level-2-regexp))))
(define-key global-map (kbd "A-s 3")
  (lambda () (interactive) (message (last-element level-3-regexp))))

;; useful insertions
(define-key global-map (kbd "A-w 0")
  (lambda () (interactive) (insert (last-element contents-level-regexp))))
(define-key global-map (kbd "A-w 1")
  (lambda () (interactive) (insert (last-element level-1-regexp))))
(define-key global-map (kbd "A-w 2")
  (lambda () (interactive) (insert (last-element level-2-regexp))))
(define-key global-map (kbd "A-w 3")
  (lambda () (interactive) (insert (last-element level-3-regexp))))

;; misc editing
(define-key global-map (kbd "A-a") 'cua-set-mark)
(define-key global-map (kbd "A-<SPC>") 'hippie-expand)
(define-key global-map (kbd "<A-backspace>") 'undo)
(define-key global-map (kbd "A-d A-d") 'exchange-point-and-mark)
(define-key global-map (kbd "A-d g") 'goto-line)
(define-key global-map (kbd "A-d G") 'goto-char)
(define-key global-map (kbd "A-d '") 'toggle-fill-column)

(define-key global-map (kbd "<A-mouse-4>") 'text-scale-increase)
(define-key global-map (kbd "<A-mouse-5>") 'text-scale-decrease)

(define-key global-map (kbd "<A-mouse-3>") 'artist-mode)

;; code navigation
(define-key global-map (kbd "A-s A-i") 'navigate-1-prev)
(define-key global-map (kbd "A-s A-k") 'navigate-1-next)
(define-key global-map (kbd "A-s A-u") 'navigate-2-prev)
(define-key global-map (kbd "A-s A-o") 'navigate-2-next)
(define-key global-map (kbd "A-s A-j") 'navigate-3-prev)
(define-key global-map (kbd "A-s A-l") 'navigate-3-next)

;; misc buffer-related
(define-key global-map (kbd "A-e A-d ,") 'copy-buffer-file-name)
(define-key global-map (kbd "A-e A-d <") 
  (lambda () (interactive) (copy-buffer-file-name t)))


;;; file- and buffer-related things

;; windows splitting and merging
(define-key global-map (kbd "A-e A-;") 'split-window-horizontally)
(define-key global-map (kbd "A-e A-/") 'split-window-vertically)
(define-key global-map (kbd "A-e <A-backspace>") 'delete-window)
(define-key global-map (kbd "A-e <backspace>") 'delete-other-windows)

;; windows resizing
(define-key global-map (kbd "A-w A-/")
  '(lambda () (interactive) (enlarge-window 1)))
(define-key global-map (kbd "A-w A-;")
  '(lambda () (interactive) (enlarge-window-horizontally 1)))
(define-key global-map (kbd "A-w A-'") 'balance-windows-area)


;; windows/buffer motion
(define-key global-map (kbd "A-e A-i") 'windmove-up)
(define-key global-map (kbd "A-e A-k") 'windmove-down)
(define-key global-map (kbd "A-e A-j") 'windmove-left)
(define-key global-map (kbd "A-e A-l") 'windmove-right)
(define-key global-map (kbd "A-w A-i") 'windmove-buf-move-up)
(define-key global-map (kbd "A-w A-k") 'windmove-buf-move-down)
(define-key global-map (kbd "A-w A-j") 'windmove-buf-move-left)
(define-key global-map (kbd "A-w A-l") 'windmove-buf-move-right)
(define-key global-map (kbd "A-e i") 'windmove-buf-swap-up)
(define-key global-map (kbd "A-e k") 'windmove-buf-swap-down)
(define-key global-map (kbd "A-e j") 'windmove-buf-swap-left)
(define-key global-map (kbd "A-e l") 'windmove-buf-swap-right)
(define-key global-map (kbd "A-w i") 'windmove-buf-copy-and-move-up)
(define-key global-map (kbd "A-w k") 'windmove-buf-copy-and-move-down)
(define-key global-map (kbd "A-w j") 'windmove-buf-copy-and-move-left)
(define-key global-map (kbd "A-w l") 'windmove-buf-copy-and-move-right)

;; accessing files and buffers
(define-key global-map (kbd "A-e A-u") 
  '(lambda () (interactive) (dired default-directory)))
(define-key global-map (kbd "A-e A-y") 'ido-find-file)
(define-key global-map (kbd "A-e A-h") 'switch-to-buffer)
(define-key global-map (kbd "A-e A-n") 'save-buffer)
(define-key global-map (kbd "A-e A-m") 'save-and-kill-buffer)
(define-key global-map (kbd "A-e A-p") 'wrapped-revert-buffer)

(define-key global-map (kbd "A-e u") 'ido-dired)
(define-key global-map (kbd "A-e y") 'wrapped-find-file)
(define-key global-map (kbd "A-e h") 'ibuffer)
(define-key global-map (kbd "A-e n") 'wrapped-write-file)
(define-key global-map (kbd "A-e m") 'wrapped-kill-buffer)

(define-key global-map (kbd "A-e RET") 'set-project-dir)

(define-key global-map (kbd "A-e A-M") 'save-and-kill-buffer-and-window)
(define-key global-map (kbd "A-e M") 'kill-buffer-and-window)
(define-key global-map (kbd "A-e U") 
  '(lambda () (interactive) (dired user-emacs-directory)))


;; code elements
(define-key global-map (kbd "<A-return>") 'contents)

;; misc
(define-key global-map (kbd "A-e f") 'new-frame)
(define-key global-map (kbd "A-e s") 'multi-eshell)
(define-key global-map (kbd "A-e A-s") 'multi-eshell-switch)
(define-key global-map (kbd "A-e S") 'eshell-in-split-window)
(define-key global-map (kbd "A-e n") 'launch-nautilus-here)
(define-key global-map (kbd "A-e t") 'launch-terminal-here)
(define-key global-map (kbd "A-e e") 'ediff-buffers)
(define-key global-map (kbd "A-e E") 'ediff-buffers3)

(define-key global-map (kbd "A-q") 'keyboard-escape-quit)
(define-key global-map (kbd "A-x A-x") 'execute-extended-command)


;; compilation and debugging
(define-key global-map (kbd "A-e A-<SPC>") 'compile-dwim)
(define-key global-map (kbd "A-e <SPC>") 'compile-with-query)
(define-key global-map (kbd "A-e o") 'set-console-window)
(define-key global-map (kbd "A-e d") 'wrapped-debug)
(define-key global-map (kbd "A-e A-.") 'next-error)
(define-key global-map (kbd "A-e A-,") 'previous-error)
(define-key global-map (kbd "A-e ,") 'first-error)


;;; END

(provide 'global-keys)
