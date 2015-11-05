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
(define-key global-map (kbd "A-N") 'fill-paragraph)

(define-key global-map (kbd "A-d A-i") 'beginning-of-buffer)
(define-key global-map (kbd "A-d A-j") 'move-beginning-of-line)
(define-key global-map (kbd "A-d A-l") 'move-end-of-line)
(define-key global-map (kbd "A-d A-k") 'end-of-buffer)
(define-key global-map (kbd "A-d A-u") 'backward-paragraph)
(define-key global-map (kbd "A-d A-o") 'forward-paragraph)
(define-key global-map (kbd "A-d A-y") 'subword-backward-kill)
(define-key global-map (kbd "A-d A-h") 'subword-kill)
(define-key global-map (kbd "A-d A-l") 'move-end-of-line)

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

(define-key global-map (kbd "A-d 7") 'highlight-symbol-query-replace)
(define-key global-map (kbd "A-d &") 'query-replace-regexp)

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
(define-key global-map (kbd "A-e 0")
  (lambda () (interactive) (insert (last-element contents-level-regexp))))
(define-key global-map (kbd "A-e 1")
  (lambda () (interactive) (insert (last-element level-1-regexp))))
(define-key global-map (kbd "A-e 2")
  (lambda () (interactive) (insert (last-element level-2-regexp))))
(define-key global-map (kbd "A-e 3")
  (lambda () (interactive) (insert (last-element level-3-regexp))))

;; misc editing
(define-key global-map (kbd "A-<SPC>") 'hippie-expand)
(define-key global-map (kbd "<A-backspace>") 'undo)
(define-key global-map (kbd "A-d A-d") 'cua-set-mark)
(define-key global-map (kbd "A-d A-s") 'exchange-point-and-mark)
(define-key global-map (kbd "A-d g") 'goto-line)
(define-key global-map (kbd "A-d G") 'goto-char)
(define-key global-map (kbd "A-d '") 'toggle-fill-column)

(define-key global-map (kbd "<A-mouse-4>") 'text-scale-increase)
(define-key global-map (kbd "<A-mouse-5>") 'text-scale-decrease)

(define-key global-map (kbd "<A-mouse-3>") 'artist-mode)


;; windows resizing
(define-key global-map (kbd "A-c A-p")
  '(lambda () (interactive) (enlarge-window 1)))
(define-key global-map (kbd "A-c A-;")
  '(lambda () (interactive) (enlarge-window-horizontally 1)))
(define-key global-map (kbd "A-c '") 'balance-windows-area)

;; code navigation
(define-key global-map (kbd "A-s A-i") 'navigate-1-prev)
(define-key global-map (kbd "A-s A-k") 'navigate-1-next)
(define-key global-map (kbd "A-s A-u") 'navigate-2-prev)
(define-key global-map (kbd "A-s A-o") 'navigate-2-next)
(define-key global-map (kbd "A-s A-j") 'navigate-3-prev)
(define-key global-map (kbd "A-s A-l") 'navigate-3-next)

;; misc buffer-related
(define-key global-map (kbd "A-c A-d ,") 'copy-buffer-file-name)
(define-key global-map (kbd "A-c A-d <") 
  (lambda () (interactive) (copy-buffer-file-name t)))


;;; ALT-KEYS: file- and buffer-related things

;; windows splitting and merging
(define-key global-map (kbd "A-c ;") 'split-window-horizontally)
(define-key global-map (kbd "A-c p") 'split-window-vertically)
(define-key global-map (kbd "A-c <A-backspace>") 'delete-window)
(define-key global-map (kbd "A-c <backspace>") 'delete-other-windows)

;; windows/buffer motion
(define-key global-map (kbd "<A-up>") 'windmove-up)
(define-key global-map (kbd "<A-down>") 'windmove-down)
(define-key global-map (kbd "<A-left>") 'windmove-left)
(define-key global-map (kbd "<A-right>") 'windmove-right)
(define-key global-map (kbd "A-c <A-up>") 'windmove-buf-move-up)
(define-key global-map (kbd "A-c <A-down>") 'windmove-buf-move-down)
(define-key global-map (kbd "A-c <A-left>") 'windmove-buf-move-left)
(define-key global-map (kbd "A-c <A-right>") 'windmove-buf-move-right)
(define-key global-map (kbd "A-c <up>") 'windmove-buf-copy-and-move-up)
(define-key global-map (kbd "A-c <down>") 'windmove-buf-copy-and-move-down)
(define-key global-map (kbd "A-c <left>") 'windmove-buf-copy-and-move-left)
(define-key global-map (kbd "A-c <right>") 'windmove-buf-copy-and-move-right)
(define-key global-map (kbd "A-x <A-up>") 'windmove-buf-swap-up)
(define-key global-map (kbd "A-x <A-down>") 'windmove-buf-swap-down)
(define-key global-map (kbd "A-x <A-left>") 'windmove-buf-swap-left)
(define-key global-map (kbd "A-x <A-right>") 'windmove-buf-swap-right)
(define-key global-map (kbd "A-x <up>") 'windmove-buf-copy-and-throw-up)
(define-key global-map (kbd "A-x <down>") 'windmove-buf-copy-and-throw-down)
(define-key global-map (kbd "A-x <left>") 'windmove-buf-copy-and-throw-left)
(define-key global-map (kbd "A-x <right>") 'windmove-buf-copy-and-throw-right)

;; accessing files and buffers
(define-key global-map (kbd "A-c A-u") 
  '(lambda () (interactive) (dired default-directory)))
(define-key global-map (kbd "A-c A-o") 'wrapped-revert-buffer)
(define-key global-map (kbd "A-c A-y") 'ido-find-file)
(define-key global-map (kbd "A-c A-h") 'switch-to-buffer)
(define-key global-map (kbd "A-c A-n") 'save-buffer)
(define-key global-map (kbd "A-c A-m") 'save-and-kill-buffer)

(define-key global-map (kbd "A-c u") 'ido-dired)
(define-key global-map (kbd "A-c y") 'wrapped-find-file)
(define-key global-map (kbd "A-c h") 'ibuffer)
(define-key global-map (kbd "A-c n") 'wrapped-write-file)
(define-key global-map (kbd "A-c m") 'wrapped-kill-buffer)

(define-key global-map (kbd "A-c RET") 'set-project-dir)

(define-key global-map (kbd "A-x A-m") 'save-and-kill-buffer-and-window)
(define-key global-map (kbd "A-x m") 'kill-buffer-and-window)
(define-key global-map (kbd "A-x u") 
  '(lambda () (interactive) (dired user-emacs-directory)))


;; code elements
(define-key global-map (kbd "<A-return>") 'contents)

;; misc
(define-key global-map (kbd "A-c f") 'new-frame)
(define-key global-map (kbd "A-q") 'keyboard-escape-quit)
(define-key global-map (kbd "A-c A-s") 'shell)
(define-key global-map (kbd "A-c s") 'shell-in-split-window)
(define-key global-map (kbd "A-c n") 'launch-nautilus-here)
(define-key global-map (kbd "A-c t") 'launch-terminal-here)
(define-key global-map (kbd "A-c e") 'ediff-buffers)
(define-key global-map (kbd "A-c E") 'ediff-buffers3)
(define-key global-map (kbd "A-x A-x") 'execute-extended-command)

;; compilation and debugging
(define-key global-map (kbd "A-c A-<SPC>") 'compile-dwim)
(define-key global-map (kbd "A-c <SPC>") 'compile-with-query)
(define-key global-map (kbd "A-c o") 'set-console-window)
(define-key global-map (kbd "A-c d") 'wrapped-debug)
(define-key global-map (kbd "A-c A-.") 'next-error)
(define-key global-map (kbd "A-c A-,") 'previous-error)
(define-key global-map (kbd "A-c ,") 'first-error)


;;; END

(provide 'global-keys)
