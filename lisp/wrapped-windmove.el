(require 'windmove)

(defun windmove-helper-start ()
  (setq start-window (selected-window))
  (setq start-buffer (window-buffer start-window)))

(defun windmove-helper-mid ()
  (setq target-window (selected-window))
  (if (or (eq target-window start-window)
	  (string-match "^ \\*Minibuf"
			(buffer-name (window-buffer target-window))))
      (error "Frame limit")))


;; buf-move: puts the current buffer in another window, and the previous
;;  buffer on the stack comes to the top inthe starting window.
(defun windmove-buf-move-helper ()
  (windmove-helper-mid)
  ;; go back to the starting window
  (select-frame (window-frame start-window))
  (select-window start-window)
  (when (not windmove-copy-buf-flag)
    ;; in starting window, bury starting buffer and let
    ;; the last buffer surface.  This action not needed if copying
    ;; rather than moving the buffer
    (bury-buffer start-buffer)
    (switch-to-buffer (other-buffer)))
  ;; make starting buffer current in other window
  (set-window-buffer target-window start-buffer)
  (select-frame (window-frame target-window))
  (select-window target-window)
  ;; finally, reset copy flag
  (setq windmove-copy-buf-flag nil))

(defun windmove-buf-move-up ()
  (interactive)
  (windmove-helper-start)
  (windmove-up)
  (windmove-buf-move-helper))

(defun windmove-buf-move-down ()
  (interactive)
  (windmove-helper-start)
  (windmove-down)
  (windmove-buf-move-helper))

(defun windmove-buf-move-left ()
  (interactive)
  (windmove-helper-start)
  (windmove-left)
  (windmove-buf-move-helper))

(defun windmove-buf-move-right ()
  (interactive)
  (windmove-helper-start)
  (windmove-right)
  (windmove-buf-move-helper))


;; buf-throw: as buf-move, but cursor stays in the starting window.
;; note: could not get buf-throw-helper to do this,
;;  so use windmove-buf-move-helper and move back after invoking.

(defun windmove-buf-throw-up ()
  (interactive)
  (windmove-helper-start)
  (windmove-up)
  (windmove-buf-move-helper)
  (windmove-down))

(defun windmove-buf-throw-down ()
  (interactive)
  (windmove-helper-start)
  (windmove-down)
  (windmove-buf-move-helper)
  (windmove-up))

(defun windmove-buf-throw-left ()
  (interactive)
  (windmove-helper-start)
  (windmove-left)
  (windmove-buf-move-helper)
  (windmove-right))

(defun windmove-buf-throw-right ()
  (interactive)
  (windmove-helper-start)
  (windmove-right)
  (windmove-buf-move-helper)
  (windmove-left))


;; buf-swap: swaps buffers between windows
(defun windmove-buf-swap-helper ()
  (windmove-helper-mid)
  ;; make target buffer current in starting window
  (set-window-buffer start-window (window-buffer target-window))  
  ;; make starting buffer current in other window
  (set-window-buffer target-window start-buffer))

(defun windmove-buf-swap-up ()
  (interactive)
  (windmove-helper-start)
  (windmove-up)
  (windmove-buf-swap-helper))

(defun windmove-buf-swap-down ()
  (interactive)
  (windmove-helper-start)
  (windmove-down)
  (windmove-buf-swap-helper))

(defun windmove-buf-swap-left ()
  (interactive)
  (windmove-helper-start)
  (windmove-left)
  (windmove-buf-swap-helper))

(defun windmove-buf-swap-right ()
  (interactive)
  (windmove-helper-start)
  (windmove-right)
  (windmove-buf-swap-helper))


;; global variable toggled by windmove-buf-copy 
(setq-default windmove-copy-buf-flag nil)

;; When this is activated, the next move or throw operation copies the
;; current buffer rather than transferring it.;
(defun windmove-buf-copy ()
  (interactive)
  (if (not windmove-copy-buf-flag)
      (progn
	(setq windmove-copy-buf-flag t)    
	;; (message "Buffer copied")
	)
      (progn
	(setq windmove-copy-buf-flag nil)    
	;; (message "Buffer copy cancelled")
	)))	

;; copy-and-move: same as move, but buffer is duplicated
(defun windmove-buf-copy-and-move-up ()
  (interactive)
  (windmove-buf-copy)
  (windmove-helper-start)
  (windmove-up)
  (windmove-buf-move-helper))

(defun windmove-buf-copy-and-move-down ()
  (interactive)
  (windmove-buf-copy)
  (windmove-helper-start)
  (windmove-down)
  (windmove-buf-move-helper))

(defun windmove-buf-copy-and-move-left ()
  (interactive)
  (windmove-buf-copy)
  (windmove-helper-start)
  (windmove-left)
  (windmove-buf-move-helper))

(defun windmove-buf-copy-and-move-right ()
  (interactive)
  (windmove-buf-copy)
  (windmove-helper-start)
  (windmove-right)
  (windmove-buf-move-helper))

;; copy-and-throw: same as throw, but buffer is duplicated
(defun windmove-buf-copy-and-throw-up ()
  (interactive)
  (windmove-buf-copy)
  (windmove-helper-start)
  (windmove-up)
  (windmove-buf-move-helper)
  (windmove-down))

(defun windmove-buf-copy-and-throw-down ()
  (interactive)
  (windmove-buf-copy)
  (windmove-helper-start)
  (windmove-down)
  (windmove-buf-move-helper)
  (windmove-up))

(defun windmove-buf-copy-and-throw-left ()
  (interactive)
  (windmove-buf-copy)
  (windmove-helper-start)
  (windmove-left)
  (windmove-buf-move-helper)
  (windmove-right))

(defun windmove-buf-copy-and-throw-right ()
  (interactive)
  (windmove-buf-copy)
  (windmove-helper-start)
  (windmove-right)
  (windmove-buf-move-helper)
  (windmove-left))


(provide 'wrapped-windmove)

