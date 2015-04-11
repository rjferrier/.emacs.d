(require 'global-defs)
(require 'whole-line-or-region)
(require 'wrapped-windmove)
(require 'wrapped-highlight-symbol)
;; (require 'lockcaps)


;;; HYPER-KEYS: text navigation and editing

;; basic functions
(define-key global-map (kbd "H-i") 'previous-line)
(define-key global-map (kbd "H-j") 'backward-char)
(define-key global-map (kbd "H-k") 'next-line)
(define-key global-map (kbd "H-l") 'forward-char)
(define-key global-map (kbd "H-u") 'move-beginning-of-line)
(define-key global-map (kbd "H-o") 'move-end-of-line)
(define-key global-map (kbd "H-y") 'backward-delete-char-untabify)
(define-key global-map (kbd "H-h") 'delete-char)
(define-key global-map (kbd "H-n") 'reindent-then-newline-and-indent)
(define-key global-map (kbd "H-.") 'whole-line-or-region-kill-region)
(define-key global-map (kbd "H-,") 'whole-line-or-region-kill-ring-save)
(define-key global-map (kbd "H-m") 'whole-line-or-region-yank)
(define-key global-map (kbd "H-;") 'comment-dwim)

;; select text
(define-key global-map (kbd "H-d H-i") 'previous-line-with-mark)
(define-key global-map (kbd "H-d H-j") 'backward-char-with-mark)
(define-key global-map (kbd "H-d H-k") 'next-line-with-mark)
(define-key global-map (kbd "H-d H-l") 'forward-char-with-mark)
(define-key global-map (kbd "H-d H-u") 'beginning-of-line-with-mark)
(define-key global-map (kbd "H-d H-o") 'end-of-line-with-mark)

;; search and replace
(define-key global-map (kbd "H-7") 'wrapped-next-symbol)
(define-key global-map (kbd "H-&") 'wrapped-prev-symbol)
(define-key global-map (kbd "H-8") 'isearch-forward-regexp)
(define-key global-map (kbd "H-*") 'isearch-backward-regexp)
(add-hook
 'isearch-mode-hook
 (lambda ()
   (define-key isearch-mode-map (kbd "H-8") 'isearch-repeat-forward))
   (define-key isearch-mode-map (kbd "H-*") 'isearch-repeat-backward))
(define-key global-map (kbd "H-d H-7") 'highlight-symbol-at-point)

;; misc
(define-key global-map (kbd "H-<SPC>") 'hippie-expand)
(define-key global-map (kbd "<H-backspace>") 'undo)
(define-key global-map (kbd "H-d ;") 'comment-set-column)
(define-key global-map (kbd "H-d H-d") 'exchange-point-and-mark)
;; ;; there seems to be a problem with lockcaps-mode...
;; (define-key global-map (kbd "H-f") 'lockcaps-mode)

(define-key global-map (kbd "H-s H-j") 
  '(lambda () (interactive) (insert-chars '("a" "b"))))


;;; ALT-HYPER-KEYS: larger movements and more advanced editing

;; basic functions
(define-key global-map (kbd "A-H-i") 'backward-paragraph)
(define-key global-map (kbd "A-H-j") 'subword-backward)
(define-key global-map (kbd "A-H-k") 'forward-paragraph)
(define-key global-map (kbd "A-H-l") 'subword-forward)
(define-key global-map (kbd "A-H-u") 'beginning-of-buffer)
(define-key global-map (kbd "A-H-o") 'end-of-buffer)
(define-key global-map (kbd "A-H-y") 'subword-backward-kill)
(define-key global-map (kbd "A-H-h") 'subword-kill)
(define-key global-map (kbd "A-H-n") 'fill-paragraph)
(define-key global-map (kbd "A-H-m") 'yank-pop)

;; select text
(define-key global-map (kbd "A-H-d A-H-i") 'backward-paragraph-with-mark)
(define-key global-map (kbd "A-H-d A-H-j") 'backward-word-with-mark)
(define-key global-map (kbd "A-H-d A-H-k") 'forward-paragraph-with-mark)
(define-key global-map (kbd "A-H-d A-H-l") 'forward-word-with-mark)
(define-key global-map (kbd "A-H-d A-H-u") 'beginning-of-buffer-with-mark)
(define-key global-map (kbd "A-H-d A-H-o") 'end-of-buffer-with-mark)

;; search and replace
(define-key global-map (kbd "A-H-7") 'highlight-symbol-query-replace)
(define-key global-map (kbd "A-H-8") 'query-replace-regexp)

;; elisp evaluation
(define-key global-map (kbd "A-H-d A-H-<SPC>") 'elisp-define-foo)
(define-key global-map (kbd "A-H-<SPC>") 'elisp-evaluate-dwim)

;; windows resizing
(define-key global-map (kbd "A-H-;")
  '(lambda () (interactive) (enlarge-window 1)))
(define-key global-map (kbd "A-H-:")
  '(lambda () (interactive) (enlarge-window -1)))
(define-key global-map (kbd "A-H-/")
  '(lambda () (interactive) (enlarge-window-horizontally 1)))
(define-key global-map (kbd "A-H-?")
  '(lambda () (interactive) (enlarge-window-horizontally -1)))
(define-key global-map (kbd "A-H-p") 'balance-windows-area)


;;; ALT-KEYS: file- and buffer-related things

;; windows splitting and merging
(define-key global-map (kbd "A-/") 'split-window-horizontally)
(define-key global-map (kbd "A-;") 'split-window-vertically)
(define-key global-map (kbd "<A-backspace>") 'delete-window)
(define-key global-map (kbd "A-c <backspace>") 'delete-other-windows)

;; windows/buffer motion
(define-key global-map (kbd "A-i") 'windmove-up)
(define-key global-map (kbd "A-k") 'windmove-down)
(define-key global-map (kbd "A-j") 'windmove-left)
(define-key global-map (kbd "A-l") 'windmove-right)
(define-key global-map (kbd "A-c A-i") 'windmove-buf-move-up)
(define-key global-map (kbd "A-c A-k") 'windmove-buf-move-down)
(define-key global-map (kbd "A-c A-j") 'windmove-buf-move-left)
(define-key global-map (kbd "A-c A-l") 'windmove-buf-move-right)
(define-key global-map (kbd "A-c i") 'windmove-buf-copy-and-move-up)
(define-key global-map (kbd "A-c k") 'windmove-buf-copy-and-move-down)
(define-key global-map (kbd "A-c j") 'windmove-buf-copy-and-move-left)
(define-key global-map (kbd "A-c l") 'windmove-buf-copy-and-move-right)
(define-key global-map (kbd "A-x A-i") 'windmove-buf-swap-up)
(define-key global-map (kbd "A-x A-k") 'windmove-buf-swap-down)
(define-key global-map (kbd "A-x A-j") 'windmove-buf-swap-left)
(define-key global-map (kbd "A-x A-l") 'windmove-buf-swap-right)
(define-key global-map (kbd "A-x i") 'windmove-buf-copy-and-throw-up)
(define-key global-map (kbd "A-x k") 'windmove-buf-copy-and-throw-down)
(define-key global-map (kbd "A-x j") 'windmove-buf-copy-and-throw-left)
(define-key global-map (kbd "A-x l") 'windmove-buf-copy-and-throw-right)

;; accessing files and buffers
(define-key global-map (kbd "A-u") 'ido-dired)
(define-key global-map (kbd "A-y") 'ido-find-file)
(define-key global-map (kbd "A-h") 'switch-to-buffer)
(define-key global-map (kbd "A-n") 'save-buffer)

(define-key global-map (kbd "A-c A-o") 'wrapped-revert-buffer)
(define-key global-map (kbd "A-c A-u") 
  '(lambda () (interactive) (dired default-directory)))
(define-key global-map (kbd "A-c A-y") 'wrapped-find-file)
(define-key global-map (kbd "A-c A-h") 'ibuffer)
(define-key global-map (kbd "A-c A-n") 'wrapped-write-file)
(define-key global-map (kbd "A-c A-m") 'save-and-kill-buffer)
(define-key global-map (kbd "A-c u") 
  '(lambda () (interactive) (dired user-emacs-directory)))
(define-key global-map (kbd "A-c m") 'wrapped-kill-buffer)

(define-key global-map (kbd "A-x A-m") 'save-and-kill-buffer-and-window)
(define-key global-map (kbd "A-x m") 'kill-buffer-and-window)


;; misc
(define-key global-map (kbd "A-f") 'new-frame)
(define-key global-map (kbd "A-q") 'keyboard-escape-quit)
(define-key global-map (kbd "A-c A-s") 'shell)
(define-key global-map (kbd "A-c s") 'shell-in-split-window)
(define-key global-map (kbd "A-c h") 'launch-nautilus-here)
(define-key global-map (kbd "A-c t") 'launch-terminal-here)
(define-key global-map (kbd "A-c e") 'ediff-buffers)
(define-key global-map (kbd "A-x A-x") 'execute-extended-command)


;; compilation and debugging
(define-key global-map (kbd "A-<SPC>") 'compile-dwim)
(define-key global-map (kbd "A-c <SPC>") 'compile-with-query)
(define-key global-map (kbd "A-d") 'wrapped-debug)
(define-key global-map (kbd "A-p") 'set-console-window)
(define-key global-map (kbd "A-m") 'first-error)
(define-key global-map (kbd "A-.") 'next-error)
(define-key global-map (kbd "A-,") 'previous-error)


;;; MORE HYPER-KEYS: for glyphs and stuff for coding

;; useful glyphs
(define-key global-map (kbd "H-s H-j") 
  '(lambda () (interactive) (insert-char ?*)))
(define-key global-map (kbd "H-s j") 
  '(lambda () (interactive) (insert-chars '(?\s ?* ?\s))))
(define-key global-map (kbd "H-s H-k") 
  '(lambda () (interactive) (insert-char ?_)))
(define-key global-map (kbd "H-s H-l")
  '(lambda () (interactive) (insert-char ?&)))
(define-key global-map (kbd "H-s H-;") 
  '(lambda () (interactive) (insert-char ?#)))
(define-key global-map (kbd "H-s H-'") 
  '(lambda () (interactive) (insert-chars '(?' ?'))
     (backward-char 1)))
(define-key global-map (kbd "H-s H-m")
  '(lambda () (interactive) (insert-char ?$)))
(define-key global-map (kbd "H-s H-,")
  '(lambda () (interactive) (insert-chars '(?< ?>)) (backward-char 1)))
(define-key global-map (kbd "H-s ,")
  '(lambda () (interactive) (insert-chars '(?< ?\s ?\s ?>))
     (backward-char 2)))
(define-key global-map (kbd "H-s H-.")
  '(lambda () (interactive) (insert-char ?~)))
(define-key global-map (kbd "H-s H-/")
  '(lambda () (interactive) (insert-char ?!)))

;; more glyphs
(define-key global-map (kbd "H-e H-j") 
  '(lambda () (interactive) (insert-char ?+)))
(define-key global-map (kbd "H-e j") 
  '(lambda () (interactive) (insert-chars '(?\s ?+ ?\s))))
(define-key global-map (kbd "H-e H-k") 
  '(lambda () (interactive) (insert-char ?-)))
(define-key global-map (kbd "H-e k") 
  '(lambda () (interactive) (insert-chars '(?\s ?- ?\s))))
(define-key global-map (kbd "H-e H-l")
  '(lambda () (interactive) (insert-char ?=)))
(define-key global-map (kbd "H-e l") 
  '(lambda () (interactive) (insert-chars '(?\s ?= ?\s))))
(define-key global-map (kbd "H-e H-;") 
  '(lambda () (interactive) (insert-char ?%)))
(define-key global-map (kbd "H-e ;") 
  '(lambda () (interactive) (insert-chars '(?\s ?% ?\s))))
(define-key global-map (kbd "H-e H-'") 
  '(lambda () (interactive) (insert-chars '(?" ?"))
     (backward-char 1)))
(define-key global-map (kbd "H-e H-m") 
  '(lambda () (interactive) (insert-char ?^)))
(define-key global-map (kbd "H-e m") 
  '(lambda () (interactive) (insert-chars '(?\s ?^ ?\s))))
(define-key global-map (kbd "H-e H-,")
  '(lambda () (interactive) (insert-chars '(?« ?»)) (backward-char 1)))
(define-key global-map (kbd "H-e ,")
  '(lambda () (interactive) (insert-chars '(?« ?\s ?\s ?»))
     (backward-char 2)))

;; curly braces
(define-key global-map (kbd "H-s H-u")
  '(lambda () (interactive) (insert-char ?{)))
(define-key global-map (kbd "H-s u")
  '(lambda () (interactive) (insert-chars '(?{ ?\s))))
(define-key global-map (kbd "H-s H-i")
  '(lambda () (interactive) (insert-chars '(?{ ?})) (backward-char 1)))
(define-key global-map (kbd "H-s i")
  '(lambda () (interactive) (insert-chars '(?{ ?\s ?\s ?}))
     (backward-char 2)))
(define-key global-map (kbd "H-s H-o")
  '(lambda () (interactive) (insert-char ?})))
(define-key global-map (kbd "H-s o")
  '(lambda () (interactive) (insert-chars '(?\s ?}))))

;; parentheses
(define-key global-map (kbd "H-e H-u")
  '(lambda () (interactive) (insert-char ?()))
(define-key global-map (kbd "H-e u")
  '(lambda () (interactive) (insert-chars '(?( ?\s))))
(define-key global-map (kbd "H-e H-i")
  '(lambda () (interactive) (insert-chars '(?( ?))) (backward-char 1)))
(define-key global-map (kbd "H-e i")
  '(lambda () (interactive) (insert-chars '(?( ?\s ?\s ?)))
     (backward-char 2)))
(define-key global-map (kbd "H-e H-o")
  '(lambda () (interactive) (insert-char ?))))
(define-key global-map (kbd "H-e o")
  '(lambda () (interactive) (insert-chars '(?\s ?)))))

;; square brackets
(define-key global-map (kbd "H-w H-u")
  '(lambda () (interactive) (insert-char ?[)))
(define-key global-map (kbd "H-w u")
  '(lambda () (interactive) (insert-chars '(?[ ?\s))))
(define-key global-map (kbd "H-w H-i")
  '(lambda () (interactive) (insert-chars '(?[ ?])) (backward-char 1)))
(define-key global-map (kbd "H-w i")
  '(lambda () (interactive) (insert-chars '(?[ ?\s ?\s ?]))
     (backward-char 2)))
(define-key global-map (kbd "H-w H-o")
  '(lambda () (interactive) (insert-char ?])))
(define-key global-map (kbd "H-w o")
  '(lambda () (interactive) (insert-chars '(?\s ?]))))

;;; END

(provide 'global-keys)
