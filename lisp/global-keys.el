(require 'global-defs)
(require 'multi-eshell)
(require 'whole-line-or-region)
(require 'wrapped-windmove)
(require 'wrapped-highlight-symbol)
(require 'zoom-frm)

(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map (kbd "M-f") 'nil)
    (define-key map (kbd "M-e") 'nil)
    (define-key map (kbd "M-w") 'nil)
    (define-key map (kbd "M-x") 'nil)

;;; text navigation and editing

    ;; basic functions
    (define-key map (kbd "M-i") 'previous-line)
    (define-key map (kbd "M-j") 'backward-char)
    (define-key map (kbd "M-k") 'next-line)
    (define-key map (kbd "M-l") 'forward-char)
    (define-key map (kbd "M-u") 'subword-backward)
    (define-key map (kbd "M-o") 'subword-forward)
    (define-key map (kbd "M-h") 'backward-delete-char-untabify)
    (define-key map (kbd "M-y") 'subword-backward-kill)
    (define-key map (kbd "M-n") 'undo)
    (define-key map (kbd "M-;") 'reindent-then-newline-and-indent)
    (define-key map (kbd "M-.") 'whole-line-or-region-kill-region)
    (define-key map (kbd "M-,") 'whole-line-or-region-kill-ring-save)
    (define-key map (kbd "M-m") 'whole-line-or-region-yank)
    (define-key map (kbd "M-H") 'delete-char)
    (define-key map (kbd "M-Y") 'subword-kill)
    (define-key map (kbd "M-N") 'indent-and-next-line)
    (define-key map (kbd "M-M") 'yank-pop)

    (define-key map (kbd "M-d M-i") 'beginning-of-buffer)
    (define-key map (kbd "M-d M-j") 'move-beginning-of-line)
    (define-key map (kbd "M-d M-l") 'move-end-of-line)
    (define-key map (kbd "M-d M-k") 'end-of-buffer)
    (define-key map (kbd "M-d M-u") 'backward-paragraph)
    (define-key map (kbd "M-d M-o") 'forward-paragraph)
    (define-key map (kbd "M-d M-l") 'move-end-of-line)

    (define-key map (kbd "M-d M-n") 'wrapped-join-line)
    (define-key map (kbd "M-d n") 'split-line)

    (define-key map (kbd "M-7") 'backward-paragraph)
    (define-key map (kbd "M-8") 'forward-paragraph)

    (define-key map (kbd "M-d M-;") 'fill-paragraph)
    (define-key map (kbd "M-d ;") 'unfill-paragraph)
    (define-key map (kbd "M-d :") 'set-fill-column)

    (define-key map (kbd "M-f M-/") 'comment-dwim)
    (define-key map (kbd "M-f /") 'comment-set-column)

    ;; select text
    (define-key map (kbd "M-I") 'previous-line-with-mark)
    (define-key map (kbd "M-J") 'backward-char-with-mark)
    (define-key map (kbd "M-K") 'next-line-with-mark)
    (define-key map (kbd "M-L") 'forward-char-with-mark)
    (define-key map (kbd "M-U") 'backward-word-with-mark)
    (define-key map (kbd "M-O") 'forward-word-with-mark)

    (define-key map (kbd "M-d M-I") 'beginning-of-buffer-with-mark)
    (define-key map (kbd "M-d M-J") 'beginning-of-line-with-mark)
    (define-key map (kbd "M-d M-K") 'end-of-buffer-with-mark)
    (define-key map (kbd "M-d M-L") 'end-of-line-with-mark)
    (define-key map (kbd "M-d M-U") 'backward-paragraph-with-mark)
    (define-key map (kbd "M-d M-O") 'forward-paragraph-with-mark)

    (define-key map (kbd "M-&") 'backward-paragraph-with-mark)
    (define-key map (kbd "M-*") 'forward-paragraph-with-mark)

    (define-key map (kbd "M-D M-I") 'beginning-of-buffer-with-mark)
    (define-key map (kbd "M-D M-J") 'beginning-of-line-with-mark)
    (define-key map (kbd "M-D M-K") 'end-of-buffer-with-mark)
    (define-key map (kbd "M-D M-L") 'end-of-line-with-mark)
    (define-key map (kbd "M-D M-U") 'backward-paragraph-with-mark)
    (define-key map (kbd "M-D M-O") 'forward-paragraph-with-mark)

    ;; search and replace
    (define-key map (kbd "M-d M-h") 'isearch-forward-regexp)
    (define-key map (kbd "M-d M-H") 'isearch-backward-regexp)
    (add-hook
     'isearch-mode-hook
     (lambda ()
       (define-key isearch-mode-map (kbd "M-k") 'isearch-repeat-forward))
     (define-key isearch-mode-map (kbd "M-i") 'isearch-repeat-backward))
    (define-key map (kbd "M-d h") 'query-replace-regexp)
    (define-key map (kbd "M-d H") 'query-replace-regexp)

    ;; symbols
    (define-key map (kbd "M-f M-9") 'highlight-symbol-at-point)
    (define-key map (kbd "M-f 9") 'highlight-symbol-query-replace)
    (define-key map (kbd "M-9") 'wrapped-next-symbol)
    (define-key map (kbd "M-(") 'wrapped-prev-symbol)

    ;; useful messages
    (define-key map (kbd "M-e f")
      (lambda () (interactive) (insert (buffer-file-name))))
    (define-key map (kbd "M-e t")
      (lambda () (interactive) (insert (custom-time-stamp))))
    (define-key map (kbd "M-e 0")
      (lambda () (interactive) (message (last-element contents-level-regexp))))
    (define-key map (kbd "M-e 1")
      (lambda () (interactive) (message (last-element level-1-regexp))))
    (define-key map (kbd "M-e 2")
      (lambda () (interactive) (message (last-element level-2-regexp))))
    (define-key map (kbd "M-e 3")
      (lambda () (interactive) (message (last-element level-3-regexp))))

    ;; useful insertions
    (define-key map (kbd "M-w 0")
      (lambda () (interactive) (insert (last-element contents-level-regexp))))
    (define-key map (kbd "M-w 1")
      (lambda () (interactive) (insert (last-element level-1-regexp))))
    (define-key map (kbd "M-w 2")
      (lambda () (interactive) (insert (last-element level-2-regexp))))
    (define-key map (kbd "M-w 3")
      (lambda () (interactive) (insert (last-element level-3-regexp))))

    ;; misc editing
    (define-key map (kbd "M-s") 'cua-set-mark)
    (define-key map (kbd "M-S") 'mark-whole-buffer)
    (define-key map (kbd "M-d M-s") 'cua-set-rectangle-mark)
    (define-key map (kbd "M-SPC") 'hippie-expand)
    (define-key map (kbd "M-d g") 'goto-line)
    (define-key map (kbd "M-d G") 'goto-char)

    (define-key map (kbd "M-RET") 'push-mark-no-activate)
    (define-key map (kbd "M-p") 'back-button-global-backward)
    (define-key map (kbd "M-P") 'back-button-global-forward)

    (define-key map (kbd "<M-mouse-4>") 'text-scale-increase)
    (define-key map (kbd "<M-mouse-5>") 'text-scale-decrease)

    (define-key map (kbd "<M-mouse-3>") 'artist-mode)

    (define-key map (kbd "M-=") 'zoom-in)
    (define-key map (kbd "M--") 'zoom-out)

    ;; code navigation
    (define-key map (kbd "M-f M-i") 'navigate-1-prev)
    (define-key map (kbd "M-f M-k") 'navigate-1-next)
    (define-key map (kbd "M-f M-u") 'navigate-2-prev)
    (define-key map (kbd "M-f M-o") 'navigate-2-next)
    (define-key map (kbd "M-f M-j") 'navigate-3-prev)
    (define-key map (kbd "M-f M-l") 'navigate-3-next)

    ;; misc buffer-related
    (define-key map (kbd "M-f M-d ,") 'copy-buffer-file-name)
    (define-key map (kbd "M-f M-d <")
      (lambda () (interactive) (copy-buffer-file-name t)))

;;; file- and buffer-related things

    ;; windows splitting and merging
    (define-key map (kbd "M-e M-'") 'split-window-horizontally)
    (define-key map (kbd "M-e M-#") 'split-window-vertically)
    (define-key map (kbd "M-e M-;") 'delete-window)
    (define-key map (kbd "M-e ;") 'delete-other-windows)

    ;; windows resizing
    (define-key map (kbd "M-#") '(lambda () (interactive) (enlarge-window 1)))
    (define-key map (kbd "M-'") '(lambda () (interactive) (enlarge-window-horizontally 1)))
    (define-key map (kbd "M-~") '(lambda () (interactive) (enlarge-window -1)))
    (define-key map (kbd "M-@") '(lambda () (interactive) (enlarge-window-horizontally -1)))
    (define-key map (kbd "M-e :") 'balance-windows-area)


    ;; window motion
    (define-key map (kbd "M-e M-i") 'windmove-up)
    (define-key map (kbd "M-e M-k") 'windmove-down)
    (define-key map (kbd "M-e M-j") 'windmove-left)
    (define-key map (kbd "M-e M-l") 'windmove-right)
    (define-key map (kbd "M-e i") 'windmove-buf-move-up)
    (define-key map (kbd "M-e k") 'windmove-buf-move-down)
    (define-key map (kbd "M-e j") 'windmove-buf-move-left)
    (define-key map (kbd "M-e l") 'windmove-buf-move-right)
    (define-key map (kbd "M-w M-i") 'windmove-buf-swap-up)
    (define-key map (kbd "M-w M-k") 'windmove-buf-swap-down)
    (define-key map (kbd "M-w M-j") 'windmove-buf-swap-left)
    (define-key map (kbd "M-w M-l") 'windmove-buf-swap-right)
    (define-key map (kbd "M-w i") 'windmove-buf-copy-and-move-up)
    (define-key map (kbd "M-w k") 'windmove-buf-copy-and-move-down)
    (define-key map (kbd "M-w j") 'windmove-buf-copy-and-move-left)
    (define-key map (kbd "M-w l") 'windmove-buf-copy-and-move-right)


    ;; accessing files and buffers
    (define-key map (kbd "M-]") 'next-buffer)
    (define-key map (kbd "M-[") 'previous-buffer)
    (define-key map (kbd "M-e M-p") 'switch-to-buffer)
    (define-key map (kbd "M-e M-o") 'next-buffer)
    (define-key map (kbd "M-e M-u") 'previous-buffer)

    (define-key map (kbd "M-e M-y") 'wrapped-find-file)
    (define-key map (kbd "M-e M-h") 'ido-find-file)
    (define-key map (kbd "M-e M-n") 'save-buffer)
    (define-key map (kbd "M-e M-,") 'wrapped-revert-buffer)
    (define-key map (kbd "M-e M-.") 'save-and-kill-buffer)

    (define-key map (kbd "M-e p") 'ibuffer)
    (define-key map (kbd "M-e y") '(lambda () (interactive)
					    (dired project-directory)))
    (define-key map (kbd "M-e Y") 'set-project-dir)
    (define-key map (kbd "M-e h") '(lambda () (interactive)
					    (dired default-directory)))
    (define-key map (kbd "M-e H") 'ido-dired)
    (define-key map (kbd "M-e n") 'wrapped-write-file)
    (define-key map (kbd "M-e N") 'save-some-buffers)
    (define-key map (kbd "M-e .") 'wrapped-kill-buffer)

    (define-key map (kbd "M-w M-.") 'save-and-kill-buffer-and-window)
    (define-key map (kbd "M-w .") 'kill-buffer-and-window)


    ;; code elements
    (define-key map (kbd "<M-f12>") 'contents)

    ;; elisp evaluation and command execution
    (define-key map (kbd "M-d <M-return>") 'elisp-evaluate-dwim)
    (define-key map (kbd "M-d RET") 'elisp-define-foo)

    (define-key map (kbd "<M-return>") 'execute-extended-command)


    ;; misc
    (define-key map (kbd "M-e e") 'new-frame)
    (define-key map (kbd "M-e d") 'ediff-buffers)
    (define-key map (kbd "M-e D") 'ediff-buffers3)

    (define-key map (kbd "M-q") 'keyboard-escape-quit)

    ;; compilation and debugging
    (define-key map (kbd "M-e <M-return>") 'compile-dwim)
    (define-key map (kbd "M-e RET") 'compile-with-query)
    (define-key map (kbd "M-e c") 'set-console-window)

    (define-key map (kbd "M-w RET") 'wrapped-debug)

    (define-key map (kbd "M-f M-k") 'next-error)
    (define-key map (kbd "M-f M-i") 'previous-error)
    (define-key map (kbd "M-f i") 'first-error)

    ;; terminals
    (define-key map (kbd "M-f M-f") 'multi-eshell-switch)
    (define-key map (kbd "M-f f") 'multi-eshell)
    (define-key map (kbd "M-f F") 'eshell-in-split-window)

    ;; external things
    (define-key map (kbd "M-x n") 'launch-nautilus-here)
    (define-key map (kbd "M-x t") 'launch-terminal-here)

    map)
  "my-keys-minor-mode keymap.")

;;; END

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

(my-keys-minor-mode 1)

(provide 'global-keys)
