;;; lockcaps.el --- caps lock mode for Emacs 

;; Copyright (C) 2001 John Paul Wallington

;; Author:  John Paul Wallington <address@hidden>
;; Created: 5 Sep 2001
;; Version: 0.9, 27 Sep 2001
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.


;;; Commentary:

;; Simulates Caps Lock.  Inspired by Hemlock on CMU Common Lisp and
;; capslock.el, a quick and dirty Emacs 19 caps-lock minor mode
;; by Eberhard Mattes <address@hidden>.

;; Thanks to:
;; Pavel Janík for suggesting supporting input methods, although the
;; resultant brain-damage is entirely my fault.
;; Deepak Goel for suggesting inverting case rather than forcing upper case.
;; Jeff Dwork for suggesting making inverting/forcing an option.


;;; Code:

;; define unless if necessary
(cond ((not (fboundp 'unless))
       (defmacro unless (cond &rest body)
         "If COND yields nil, do BODY, else return nil."
         (cons 'if (cons cond (cons nil body))))
       (put 'unless 'lisp-indent-function 1)))

;; simulate customize if necessary
(unless (fboundp 'defgroup)
  (defmacro defgroup  (&rest rest) nil)
  (defmacro defcustom (symbol init docstring &rest rest)
    `(defvar ,symbol ,init ,docstring)))


(defgroup lockcaps nil
  "Simulate Caps Lock"
  :group 'convenience)

(defcustom lockcaps-all-keys nil
  "*Simulate Caps Lock for a-z only or other characters too."
  :type '(choice (const :tag "a-z only" nil)
                 (const :tag "Other characters too" t))
  :group 'lockcaps)

(defcustom lockcaps-invert nil
  "*Using Shift with lockcaps inverts case or forces upper case."
  :type '(choice (const :tag "Invert case" t)
                 (const :tag "Force upper case" nil))           
  :group 'lockcaps)


(defvar lockcaps-mode nil
  "Mode variable for lockcaps minor mode.")
(make-variable-buffer-local 'lockcaps-mode)

(defvar lockcaps-mode-map nil
  "Keymap for lockcaps minor mode.")

(defvar lockcaps-all-keys-mode nil
  "Mode variable for lockcaps-all-keys behaviour for lockcaps minor mode.")
(make-variable-buffer-local 'lockcaps-all-keys-mode)

(defvar lockcaps-all-keys-map nil
  "Keymap for all keys for lockcaps minor mode.")


(unless lockcaps-mode-map
  (setq lockcaps-mode-map (make-sparse-keymap))
  (let ((keys
         '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m 
              ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
              ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M
              ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z)))
    (while keys
      (define-key lockcaps-mode-map
        (char-to-string (car keys)) 'lockcaps-self-insert)
      (setq keys (cdr keys)))))


(unless lockcaps-all-keys-map
  (setq lockcaps-all-keys-map (make-keymap))
  (substitute-key-definition
   'self-insert-command 'lockcaps-self-insert
   lockcaps-all-keys-map global-map)
  (if (char-table-p (standard-case-table))
      (map-char-table
       #'(lambda (key value)
           (if (< 127 key)
               (define-key lockcaps-all-keys-map
                 (vector key) 'lockcaps-self-insert)))
       (standard-case-table))))


(or (assq 'lockcaps-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'lockcaps-mode lockcaps-mode-map) 
                minor-mode-map-alist)))
(or (assq 'lockcaps-all-keys-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'lockcaps-all-keys-mode lockcaps-all-keys-map) 
                minor-mode-map-alist)))
(or (assq 'lockcaps-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(lockcaps-mode " CAPS") minor-mode-alist)))


;;;###autoload 
(defun lockcaps-mode (&optional arg)
  "Toggle lockcaps minor mode."
  (interactive "P")
  (setq lockcaps-mode
        (if (null arg)
            (not lockcaps-mode)
          (> (prefix-numeric-value arg) 0)))
  (setq lockcaps-all-keys-mode (and lockcaps-mode lockcaps-all-keys))
  (force-mode-line-update))

;; [RJF: last-command-char -> last-command-event]
(defun lockcaps-self-insert (arg)
  "Insert the character you type, simulating Caps Lock."
  (interactive "*p")
  (setq last-command-event
        (if lockcaps-invert
            (if (equal (downcase last-command-event) last-command-event)
                (upcase last-command-event)
              (downcase last-command-event))
          (upcase last-command-event)))
  (self-insert-command arg))


(provide 'lockcaps)
;;; lockcaps.el ends here
