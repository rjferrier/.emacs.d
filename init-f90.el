

(defun f90-delete-indentation ()
  "Rejoins line to previous; removes ampersand"
  (interactive)
  (delete-indentation)  
  (backward-char)
  ;; delete ampersand
  (when (string= "&" (thing-at-point 'char))
    (delete-char 2))
  ;; need to do this again if a string continuation
  (when (string= "&" (thing-at-point 'char))
    (delete-char 1)))

(defun f90-delete-indentation-next-line ()
  "Rejoins next line to current; removes ampersand"
  (interactive)
  (forward-line)
  (f90-delete-indentation))

;; extensions to the above three functions whereby the remaining line
;; is auto-formatted (see next function).
(defun f90-break-line-and-auto-format ()
  (interactive)
  (f90-break-line)
  (f90-auto-format-line))
(defun f90-delete-indentation-and-auto-format ()
  (interactive)
  (f90-delete-indentation)
  (f90-auto-format-line))
(defun f90-delete-indentation-next-line-and-auto-format ()
  (interactive)
  (forward-line)
  (f90-delete-indentation-next-line))

(defun f90-auto-format-line ()
  "Auto-formats line and moves to the next line"
  (interactive)
  (back-to-indentation)
  ;; do not perform on commented-out regions.  Note that (current-word)
  ;; for some reason ignores "!", so "!!$" appears as "$"; "!>" as ">";
  ;; etc.
  (when (not (string= (current-word) "$"))
    ;; check for all other (e.g. Doxygen) comment tokens
    (if (or (string= (thing-at-point 'char) "!")
	    (string= (current-word) "!>")
	    (string= (current-word) "!<")
	    (string= (current-word) "!!"))
	(fill-paragraph)  ;; if commented region, delegate to auto fill
      (let ((flag t) (i 1) (j 1))     ;; otherwise, start a loop
	(while (and flag (< i 50))
	  (end-of-line)
	  (if (eq (current-column) 0)   ;; stop if a new line
	      (setq flag nil)
	    (progn
	      (backward-char)
	      (while (string= " " (thing-at-point 'char))
		(delete-char 1)
		(message "backward-char")
		(backward-char))   ;; delete trailing whitespace
	      (if (> (current-column) (- fill-column 1))
		  (progn	          ;; break line if too long
		    (while (and
			    (> (current-column) (- fill-column 1))
			    (< j fill-column))
		      (message "backward-word")
		      (backward-word)		    
		      (setq j (+ j 1)))
		    (backward-char)
		    ;; check that we are not terminating at string
		    ;; openers, periods or numbers
		    (while (and (or
				 (string= "0" (thing-at-point 'char))
				 (not (eq 0 (string-to-number
					     (thing-at-point 'char))))
				 (string= "." (thing-at-point 'char))
				 (string= "'" (thing-at-point 'char))
				 (string= "\"" (thing-at-point 'char)))
				(> (current-column) 0))
		      (backward-char))
		    (forward-char)
		    
		    (f90-break-line))
		;; otherwise, check continuation character
		(if (string= "&" (thing-at-point 'char))
		    (progn
		      ;; merge if appropriate		    
		      (f90-delete-indentation-next-line)
		      ;; is the line now too long?  Undo with a break
		      (end-of-line)
		      (when (> (current-column) (- fill-column 1))
			(while (and
				(> (current-column) (- fill-column 1))
				(< j fill-column))
			  (message "backward-word [2]")
			  (backward-word)   
			  (setq j (+ j 1)))
			(backward-char)
			;; check that we are not terminating at string
			;; openers, periods or numbers
			(while (and (or
				     (string= "0"
					      (thing-at-point 'char))
				     (not
				      (eq 0 (string-to-number
					     (thing-at-point 'char))))
				     (string= "."
					      (thing-at-point 'char))
				     (string= "'"
					      (thing-at-point 'char))
				     (string= "\""
					      (thing-at-point 'char)))
				    (> (current-column) 0))
			  (backward-char))

			(forward-char)
			(f90-break-line)))
		  ;; if no continuation, we have run out of things
		  ;; to do.  Terminate loop.
		  (setq flag nil)))))
	  (setq i (+ i 1))))))
  (forward-line)
  (indent-according-to-mode)
  (end-of-line))


(eval-after-load "f90"
  '(progn
     ;; source code width
     (set-fill-column 80)

     (define-key f90-mode-map (kbd "A-H-n") 'f90-auto-format-line)
     (define-key f90-mode-map (kbd "H-s y") 'f90-delete-indentation)
     (define-key f90-mode-map (kbd "H-s h") 'f90-delete-indentation-next-line)
     (define-key f90-mode-map (kbd "H-s n") 'f90-break-line)
     (define-key f90-mode-map (kbd "H-s H-y")
       'f90-delete-indentation-and-auto-format)
     (define-key f90-mode-map (kbd "H-s H-h")
       'f90-delete-indentation-next-line-and-auto-format)
     (define-key f90-mode-map (kbd "H-s H-n") 'f90-break-line-and-auto-format)
     (define-key f90-mode-map (kbd "H-s t") 'f90-expand-and-insert-type)
     ))

  
(add-hook
 'f90-mode-hook
 (lambda ()
   (progn
     (abbrev-mode 1)                       ; turn on abbreviation mode
     (turn-on-font-lock)                   ; syntax highlighting
                          
     (setq f90-auto-keyword-case 'downcase-word                          
	   f90-do-indent 3                                               
	   f90-if-indent 3                                               
	   f90-type-indent 3                                             
	   f90-program-indent 3                                          
	   f90-continuation-indent 6                                     
	   f90-comment-region "!!!"                                      
	   f90-directive-comment "!!$"                                   
	   f90-indented-comment "!"                                      
	   f90-break-before-delimiters t                                 
	   f90-beginning-ampersand t
	   f90-font-lock-keywords f90-font-lock-keywords-3)
     
   
     ;; navigation
     (set (make-local-variable 'level-1-regexp)
	  "^ *\\(\\(?:end +\\)?module.*\\|contains\\)")
     (set (make-local-variable 'level-2-regexp)
	  "^ *\\(\\(subroutine\\|function\\|type\\) +[a-zA-Z_].*\\)")
     (set (make-local-variable 'level-3-regexp)
	  "^ *\\(\\(?:[a-zA-Z_]+: *\\)?\\(do\\|if\\|select\\).*\\)")
     (set (make-local-variable 'contents-level-regexp) level-2-regexp)
     )))
