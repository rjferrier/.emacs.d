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

;; useful messages
(define-key global-map (kbd "H-s f")
  (lambda () (interactive) (insert (buffer-file-name))))
(define-key global-map (kbd "H-s t")
  (lambda () (interactive) (insert (custom-time-stamp))))
(define-key global-map (kbd "H-s 0")
  (lambda () (interactive) (message (last-element contents-level-regexp))))
(define-key global-map (kbd "H-s 1")
  (lambda () (interactive) (message (last-element level-1-regexp))))
(define-key global-map (kbd "H-s 2")
  (lambda () (interactive) (message (last-element level-2-regexp))))
(define-key global-map (kbd "H-s 3")
  (lambda () (interactive) (message (last-element level-3-regexp))))

;; useful insertions
(define-key global-map (kbd "H-e 0")
  (lambda () (interactive) (insert (last-element contents-level-regexp))))
(define-key global-map (kbd "H-e 1")
  (lambda () (interactive) (insert (last-element level-1-regexp))))
(define-key global-map (kbd "H-e 2")
  (lambda () (interactive) (insert (last-element level-2-regexp))))
(define-key global-map (kbd "H-e 3")
  (lambda () (interactive) (insert (last-element level-3-regexp))))

;; misc
(define-key global-map (kbd "H-<SPC>") 'hippie-expand)
(define-key global-map (kbd "<H-backspace>") 'undo)
(define-key global-map (kbd "H-d ;") 'comment-set-column)
(define-key global-map (kbd "H-d H-d") 'exchange-point-and-mark)
;; ;; there seems to be a problem with lockcaps-mode...
;; (define-key global-map (kbd "H-f") 'lockcaps-mode)
(define-key global-map (kbd "H-s g") 'goto-line)
(define-key global-map (kbd "H-s G") 'goto-char)

(define-key global-map (kbd "<H-mouse-3>") 'artist-mode)


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
(define-key global-map (kbd "A-H-,") 'copy-buffer-file-name)
(define-key global-map (kbd "A-H-<") 
  (lambda () (interactive) (copy-buffer-file-name t)))

;; select text
(define-key global-map (kbd "A-H-d A-H-i") 'backward-paragraph-with-mark)
(define-key global-map (kbd "A-H-d A-H-j") 'backward-word-with-mark)
(define-key global-map (kbd "A-H-d A-H-k") 'forward-paragraph-with-mark)
(define-key global-map (kbd "A-H-d A-H-l") 'forward-word-with-mark)
(define-key global-map (kbd "A-H-d A-H-u") 'beginning-of-buffer-with-mark)
(define-key global-map (kbd "A-H-d A-H-o") 'end-of-buffer-with-mark)

;; misc text editing
(define-key global-map (kbd "A-H-d A-H-n") 'toggle-fill-column)

;; search and replace
(define-key global-map (kbd "A-H-7") 'highlight-symbol-query-replace)
(define-key global-map (kbd "A-H-8") 'query-replace-regexp)

;; windows resizing
(define-key global-map (kbd "A-H-p")
  '(lambda () (interactive) (enlarge-window 1)))
(define-key global-map (kbd "A-H-P")
  '(lambda () (interactive) (enlarge-window -1)))
(define-key global-map (kbd "A-H-;")
  '(lambda () (interactive) (enlarge-window-horizontally 1)))
(define-key global-map (kbd "A-H-:")
  '(lambda () (interactive) (enlarge-window-horizontally -1)))
(define-key global-map (kbd "A-H-'") 'balance-windows-area)

;; code navigation
(define-key global-map (kbd "A-H-s A-H-i") 'navigate-up-toggle)
(define-key global-map (kbd "A-H-s A-H-k") 'navigate-down-toggle)
(define-key global-map (kbd "A-H-s A-H-j") 'navigate-left-toggle)
(define-key global-map (kbd "A-H-s A-H-l") 'navigate-right-toggle)
(define-key global-map (kbd "A-H-s A-H-u") 'navigate-further-up-toggle)
(define-key global-map (kbd "A-H-s A-H-o") 'navigate-further-down-toggle)
(define-key global-map (kbd "A-H-i") 'navigate-up)
(define-key global-map (kbd "A-H-k") 'navigate-down)
(define-key global-map (kbd "A-H-j") 'navigate-left)
(define-key global-map (kbd "A-H-l") 'navigate-right)
(define-key global-map (kbd "A-H-u") 'navigate-further-up)
(define-key global-map (kbd "A-H-o") 'navigate-further-down)


;;; ALT-KEYS: file- and buffer-related things

;; windows splitting and merging
(define-key global-map (kbd "A-;") 'split-window-horizontally)
(define-key global-map (kbd "A-p") 'split-window-vertically)
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

(define-key global-map (kbd "A-c A-u") 
  '(lambda () (interactive) (dired default-directory)))
(define-key global-map (kbd "A-c A-o") 'wrapped-revert-buffer)
(define-key global-map (kbd "A-c A-y") 'wrapped-find-file)
(define-key global-map (kbd "A-c A-h") 'ibuffer)
(define-key global-map (kbd "A-c A-n") 'wrapped-write-file)
(define-key global-map (kbd "A-c A-m") 'save-and-kill-buffer)
(define-key global-map (kbd "A-c u") 'set-project-dir)
(define-key global-map (kbd "A-c m") 'wrapped-kill-buffer)

(define-key global-map (kbd "A-x A-m") 'save-and-kill-buffer-and-window)
(define-key global-map (kbd "A-x m") 'kill-buffer-and-window)
(define-key global-map (kbd "A-x u") 
  '(lambda () (interactive) (dired user-emacs-directory)))


;; code elements
(define-key global-map (kbd "<A-return>") 'contents)

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
(define-key global-map (kbd "A-c o") 'set-console-window)
(define-key global-map (kbd "A-d") 'wrapped-debug)
(define-key global-map (kbd "A-m") 'first-error)
(define-key global-map (kbd "A-.") 'next-error)
(define-key global-map (kbd "A-,") 'previous-error)


;;; MORE HYPER-KEYS: for glyphs and stuff for coding

;; (Caution - probably better to get into the good habit of
;; touch-typing these symbols on a standard keymap.)

;; really useful glyphs
(define-key global-map (kbd "H-'")
  '(lambda () (interactive) (insert-char ?" 1))) ;"
(define-key global-map (kbd "H-/")
  '(lambda () (interactive) (insert-char ?\\ 1)))

;; glyphs commonly used for coding
(define-key global-map (kbd "H-s H-j")
  '(lambda () (interactive) (insert-char ?* 1)))
(define-key global-map (kbd "H-s j") 
  '(lambda () (interactive) (insert-chars '(?\s ?* ?\s))))
(define-key global-map (kbd "H-s H-k") 
  '(lambda () (interactive) (insert-char ?_ 1)))
(define-key global-map (kbd "H-s H-l")
  '(lambda () (interactive) (insert-char ?& 1)))
(define-key global-map (kbd "H-s H-;") 
  '(lambda () (interactive) (insert-char ?# 1)))
(define-key global-map (kbd "H-s H-'") 
  '(lambda () (interactive) (insert-char ?@ 1)))
(define-key global-map (kbd "H-s H-m")
  '(lambda () (interactive) (insert-char ?$ 1)))
(define-key global-map (kbd "H-s H-,")
  '(lambda () (interactive) (insert-chars '(?< ?>)) (backward-char 1)))
(define-key global-map (kbd "H-s ,")
  '(lambda () (interactive) (insert-chars '(?< ?\s ?\s ?>))
     (backward-char 2)))
(define-key global-map (kbd "H-s H-.")
  '(lambda () (interactive) (insert-chars '(?' ?')) (backward-char 1)))
(define-key global-map (kbd "H-s H-/")
  '(lambda () (interactive) (insert-char ?| 1)))

;; glyphs commonly used for arithmetic and punctuation
(define-key global-map (kbd "H-e H-j") 
  '(lambda () (interactive) (insert-char ?+ 1)))
(define-key global-map (kbd "H-e j") 
  '(lambda () (interactive) (insert-chars '(?\s ?+ ?\s))))
(define-key global-map (kbd "H-e H-k") 
  '(lambda () (interactive) (insert-char ?- 1)))
(define-key global-map (kbd "H-e k") 
  '(lambda () (interactive) (insert-chars '(?\s ?- ?\s))))
(define-key global-map (kbd "H-e H-l")
  '(lambda () (interactive) (insert-char ?= 1)))
(define-key global-map (kbd "H-e l") 
  '(lambda () (interactive) (insert-chars '(?\s ?= ?\s))))
(define-key global-map (kbd "H-e H-;") 
  '(lambda () (interactive) (insert-char ?% 1)))
(define-key global-map (kbd "H-e ;") 
  '(lambda () (interactive) (insert-chars '(?\s ?% ?\s))))
(define-key global-map (kbd "H-e H-'") 
  '(lambda () (interactive) (insert-char ?` 1)))
(define-key global-map (kbd "H-e H-m")
  '(lambda () (interactive) (insert-char ?^ 1)))
(define-key global-map (kbd "H-e m") 
  '(lambda () (interactive) (insert-chars '(?\s ?^ ?\s))))
(define-key global-map (kbd "H-s /")
  '(lambda () (interactive) (insert-char ?~ 1)))
(define-key global-map (kbd "H-e H-.")
  '(lambda () (interactive) (insert-chars '(?" ?")) (backward-char 1)))
(unless in-win32
  (define-key global-map (kbd "H-e H-,")
    '(lambda () (interactive) (insert-chars '(?« ?»)) (backward-char 1)))
  (define-key global-map (kbd "H-e ,")
    '(lambda () (interactive) (insert-chars '(?« ?\s ?\s ?»))
       (backward-char 2))))
(define-key global-map (kbd "H-e H-/")
  '(lambda () (interactive) (insert-char ?! 1)))

;; curly braces
(define-key global-map (kbd "H-s H-u")
  '(lambda () (interactive) (insert-char ?{ 1)))
(define-key global-map (kbd "H-s u")
  '(lambda () (interactive) (insert-chars '(?{ ?\s))))
(define-key global-map (kbd "H-s H-i")
  '(lambda () (interactive) (insert-chars '(?{ ?})) (backward-char 1)))
(define-key global-map (kbd "H-s i")
  '(lambda () (interactive) (insert-chars '(?{ ?\s ?\s ?}))
     (backward-char 2)))
(define-key global-map (kbd "H-s H-o")
  '(lambda () (interactive) (insert-char ?} 1)))
(define-key global-map (kbd "H-s o")
  '(lambda () (interactive) (insert-chars '(?\s ?}))))

;; parentheses
(define-key global-map (kbd "H-e H-u")
  '(lambda () (interactive) (insert-char ?( 1)))
(define-key global-map (kbd "H-e u")
  '(lambda () (interactive) (insert-chars '(?( ?\s))))
(define-key global-map (kbd "H-e H-i")
  '(lambda () (interactive) (insert-chars '(?( ?))) (backward-char 1)))
(define-key global-map (kbd "H-e i")
  '(lambda () (interactive) (insert-chars '(?( ?\s ?\s ?)))
     (backward-char 2)))
(define-key global-map (kbd "H-e H-o")
  '(lambda () (interactive) (insert-char ?) 1)))
(define-key global-map (kbd "H-e o")
  '(lambda () (interactive) (insert-chars '(?\s ?)))))

;; square brackets
(define-key global-map (kbd "H-w H-u")
  '(lambda () (interactive) (insert-char ?[ 1)))
(define-key global-map (kbd "H-w u")
  '(lambda () (interactive) (insert-chars '(?[ ?\s))))
(define-key global-map (kbd "H-w H-i")
  '(lambda () (interactive) (insert-chars '(?[ ?])) (backward-char 1)))
(define-key global-map (kbd "H-w i")
  '(lambda () (interactive) (insert-chars '(?[ ?\s ?\s ?]))
     (backward-char 2)))
(define-key global-map (kbd "H-w H-o")
  '(lambda () (interactive) (insert-char ?] 1)))
(define-key global-map (kbd "H-w o")
  '(lambda () (interactive) (insert-chars '(?\s ?]))))




;;; END

(provide 'global-keys)
