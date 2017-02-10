;;!emacs
;;
;; FILE:         br-site.el
;; SUMMARY:      Site OO-Browser per Emacs session initialization.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     local, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:    18-May-90
;; LAST-MOD:      9-Jun-99 at 18:05:14 by Bob Weiner
;;
;; Copyright (C) 1990-1995, 1997, 1998 BeOpen.com
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar smart-scroll-proportional t
  "*Non-nil means Smart Keys should scroll relative to current line when pressed at the end of a line.
Action Key moves current line to top of window.  Assist Key moves current
line to bottom of window.  Repeated presses then scroll up or down a
windowful.  Nil value instead ignores current line and always scrolls up or
down a windowful.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun br-setup-internal ()
  "Site customizable function to configure the OO-Browser for Emacs-based editing.
This must be run after \"br-init\" has been loaded."
  (setq br-editor-cmd nil br-ed1 nil br-ed2 nil))

(defun br-setup-external ()
  "Site customizable function to configure the OO-Browser for non-Emacs editing.
This must be run after \"br-init\" has been loaded."
  (setq br-editor-cmd "xterm"
	br-ed1 "-e" br-ed2 "vi"
	;; In general, users will want to view within the textual browser
	;; itself, even when using an external editor.
	;; br-viewer-cmd "xterm"
	;; br-vw1 "-e" br-vw2 "more"
	))

(defun br-setup-mouse-keys ()
  "Binds the Action Key within OO-Browser listing buffers under XEmacs and GNU Emacs."
  ;; Do nothing when running in batch mode.
  (if (or noninteractive (featurep 'infodock))
      nil
    (cond ;; GNU Emacs 19
     (hyperb:emacs19-p
      (if (= hyperb:mouse-buttons 2)
	  ;; With 2 mouse buttons, we have to use the left as the Action Key.
	  ;; We move the mouse-set-point command to shift-left.
	  (progn
	    (define-key br-mode-map [S-down-mouse-1] 'mouse-drag-region)
	    (define-key br-mode-map [S-mouse-1] 'mouse-set-point)
	    (define-key br-mode-map [down-mouse-1] 'action-key-depress-emacs19)
	    (define-key br-mode-map [mouse-1] 'action-mouse-key-emacs19)
	    (define-key br-mode-map [double-mouse-1] 'action-mouse-key-emacs19)
	    (define-key br-mode-map [triple-mouse-1] 'action-mouse-key-emacs19)
	    (define-key br-mode-map [vertical-line down-mouse-1]
	      'action-key-depress-emacs19)
	    (define-key br-mode-map [vertical-line mouse-1]
	      'action-mouse-key-emacs19))
	;; X
	(define-key br-mode-map [down-mouse-2]   'action-key-depress-emacs19)
	(define-key br-mode-map [mouse-2]        'action-mouse-key-emacs19)
	(define-key br-mode-map [double-mouse-2] 'action-mouse-key-emacs19)
	(define-key br-mode-map [triple-mouse-2] 'action-mouse-key-emacs19)
	(define-key br-mode-map [vertical-line down-mouse-2]
	  'action-key-depress-emacs19)
	(define-key br-mode-map [vertical-line mouse-2]
	  'action-mouse-key-emacs19)))
     ;;
     ;; XEmacs
     (hyperb:xemacs-p
      ;; Set mouse bindings under XEmacs, even if not under a window
      ;; system since it can have frames on ttys and windowed displays at
      ;; the same time.
      ;;
      (if (= hyperb:mouse-buttons 2)
	  ;; With 2 mouse buttons, we have to use the left as the Action Key.
	  ;; We move the mouse-set-point command to shift-left.
	  (progn (define-key br-mode-map '(shift button1)
		   (lookup-key global-map 'button1))
		 (define-key br-mode-map '(shift button1up)
		   (lookup-key global-map 'button1up))
		 (define-key br-mode-map 'button1     'action-key-depress)
		 (define-key br-mode-map 'button1up   'action-mouse-key))
	(define-key br-mode-map 'button2     'action-key-depress)
	(define-key br-mode-map 'button2up   'action-mouse-key))))))

(defun br-site-after-term-init ()
  (interactive)
  (if noninteractive
      (br-init-autoloads)
    (br-after-term-init)
    (br-setup-mouse-keys))
  ;;
  ;;     DON'T PUT IN br-init.el
  ;;
  (require 'br)
  (if noninteractive
      nil
    (setq c++-cpp-include-dirs '("/usr/include/")
	  c++-include-dirs 
	  (delq nil (mapcar 
		     (function (lambda (dir) (if (file-exists-p dir) dir)))
		     '("/usr/include/X11/" "/usr/openwin/include/X11/"))))))

;; Execute
(br-site-after-term-init)
(if hyperb:window-system (require 'br-tree))

;; Add global browser-related keys.
;;
;; Adds or replaces class entry in an Environment
;   (global-set-key "\C-c^" 'br-add-class-file)
;;
;; Goes to and from class viewer window
(global-set-key "\C-c\C-v" 'br-to-from-viewer)
;;
;; Completes symbols using OO-Browser Environments when available.
(global-set-key "\M-\C-i" 'br-complete-symbol)

;; Add mode-specific browser-related keys.
;;
(if (locate-file "c++-browse.elc" load-path)
    (add-hook
     'c++-mode-hook
     (function
      (lambda ()
	(define-key c++-mode-map       "\C-c\M-j" 'br-feature-edit-declaration)
	(define-key c++-mode-map       "\C-c\M-f" 'br-find)
	(define-key c++-mode-map       "\C-\M-i"  'br-complete-symbol)
	(define-key c++-mode-map       "\C-c\M-w" 'br-where)
	;; Prevent these bindings from interfering with the global
	;; OO-Browser bindings.
	(define-key c++-mode-map       "\C-c\C-o"  nil)
	(define-key c++-mode-map       "\C-c\C-v"  nil)
	(if (boundp 'c-mode-map)
	    (progn (define-key c-mode-map "\C-c\C-o"  nil)
		   (define-key c-mode-map "\C-c\C-v"  nil)))))))

(if (locate-file "eif-browse.elc" load-path)
    (add-hook
     'eiffel-mode-hook
     (function
      (lambda ()
	(define-key eiffel-mode-map    "\C-c\M-f" 'br-find)
	(define-key eiffel-mode-map    "\C-\M-i"  'br-complete-symbol)
	(define-key eiffel-mode-map    "\C-c\M-w" 'br-where)))))

(if (locate-file "java-brows.elc" load-path)
    (add-hook
     'java-mode-hook
     (function
      (lambda ()
	(define-key java-mode-map      "\C-c\M-f" 'br-find)
	(define-key java-mode-map      "\C-\M-i"  'br-complete-symbol)
	(define-key java-mode-map      "\C-c\M-w" 'br-where)
	(define-key java-mode-map      "\C-c\C-o"  nil)
	(define-key java-mode-map      "\C-c\C-v"  nil)
	(if (boundp 'c-mode-map)
	    ;; Prevent these bindings from interfering with the global
	    ;; OO-Browser bindings.
	    (progn (define-key c-mode-map "\C-c\C-o"  nil)
		   (define-key c-mode-map "\C-c\C-v"  nil)))))))

(if (locate-file "clos-brows.elc" load-path)
    (add-hook
     'lisp-mode-hook
     (function
      (lambda ()
	(define-key lisp-mode-map      "\C-c\M-f" 'br-find)
	(define-key lisp-mode-map      "\C-\M-i"  'br-complete-symbol)
	(define-key lisp-mode-map      "\C-c\M-w" 'br-where)))))

(if (locate-file "objc-brows.elc" load-path)
    (add-hook
     'objc-mode-hook
     (function
      (lambda ()
	(define-key objc-mode-map      "\C-c\M-f" 'br-find)
	(define-key objc-mode-map      "\C-\M-i"  'br-complete-symbol)
	(define-key objc-mode-map      "\C-c\M-w" 'br-where)
	(if (boundp 'c-mode-map)
	    ;; Prevent these bindings from interfering with the global
	    ;; OO-Browser bindings.
	    (progn (define-key c-mode-map "\C-c\C-o"  nil)
		   (define-key c-mode-map "\C-c\C-v"  nil)))))))

(if (locate-file "pyth-brows.elc" load-path)
    (add-hook
     'python-mode-hook
     (function
      (lambda ()
	(let ((keymap (if (and (boundp 'py-mode-map) (keymapp py-mode-map))
			  py-mode-map
			python-mode-map)))
	  (define-key keymap    "\C-c\M-f" 'br-find)
	  (define-key keymap    "\C-\M-i"  'br-complete-symbol)
	  (define-key keymap    "\C-c\M-w" 'br-where)
	  ;; Prevent these bindings from interfering with the global
	  ;; OO-Browser bindings.
	  (define-key keymap "\C-c\C-o"  nil)
	  (define-key keymap "\C-c\C-v"  nil))))))

(if (locate-file "smt-browse.elc" load-path)
    (add-hook
     'smalltalk-mode-hook
     (function
      (lambda ()
	(define-key smalltalk-mode-map "\C-c\M-f" 'br-find)
	(define-key smalltalk-mode-map "\C-\M-i"  'br-complete-symbol)
	(define-key smalltalk-mode-map "\C-c\M-w" 'br-where)))))

(provide 'br-site)
