(require 'highlight-symbol)

(defun wrapped-highlight-symbol-p ()
  (let ((symbol (highlight-symbol-get-symbol)))
    (and symbol (or (equal symbol highlight-symbol)
		    (member symbol highlight-symbol-list)))))

(defun wrapped-next-symbol ()
  (interactive)
  (if (thing-at-point 'symbol)
      (if (wrapped-highlight-symbol-p)
	  (highlight-symbol-next)
	(progn
	  (highlight-symbol-at-point)
	  (highlight-symbol-next)
	  (highlight-symbol-at-point)))
    (progn
      (message (concat "No symbol at point => Searching instead for "
		       (car kill-ring-yank-pointer) " from the kill ring"))
      (search-forward (car kill-ring-yank-pointer)))
    ))
    
(defun wrapped-prev-symbol ()
  (interactive)
  (if (thing-at-point 'symbol)
      (if (wrapped-highlight-symbol-p)
	  (highlight-symbol-prev)
	(progn
	  (highlight-symbol-at-point)
	  (highlight-symbol-prev)
	  (highlight-symbol-at-point)))
    (progn
      (message (concat "No symbol at point => Searching instead for "
		       (car kill-ring-yank-pointer) " from the kill ring"))
      (search-backward (car kill-ring-yank-pointer)))
    ))


(provide 'wrapped-highlight-symbol)
