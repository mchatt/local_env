;;!emacs
;;
;; FILE:         hmouse-key.el
;; SUMMARY:      Load "hmouse-sh.el" or "hmouse-reg.el" for Smart Key bindings.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     hypermedia, mouse
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:    30-May-94 at 00:11:57
;; LAST-MOD:     13-Jun-99 at 01:16:36 by Bob Weiner
;;
;; Copyright (C) 1994-1995, 1997  BeOpen.com
;; See the HY-COPY (Hyperbole) or BR-COPY (OO-Browser) file for license
;; information.
;;
;; This file is part of Hyperbole and the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;   Supports X, NEXTSTEP, and the Apollo DM window systems.
;;
;;   `hmouse-shift-buttons' globally binds the Action and Assist Mouse Keys
;;   to either shifted or unshifted mouse buttons.
;;
;;   `hmouse-toggle-bindings' may be bound to a key.  It switches between
;;   the Hyperbole mouse bindings and previous mouse key bindings any time
;;   after `hmouse-shift-buttons' has been called.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hversion)
(require 'hmouse-drv)
(require 'h-skip-bytec "h-skip-bytec.lsp")

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(if (or hyperb:xemacs-p hyperb:emacs19-p)
    ;; XEmacs and Emacs 19 pre-load their mouse libraries, so
    ;; we shouldn't have to require them here.
    nil
  (eval (cdr (assoc hyperb:window-system
		    '(
		      ("xterm"   . (require 'x-mouse))     ; X
		      ("next"    . (load "eterm-fns" t))   ; NeXTstep
		      ("apollo"  . (require 'apollo))      ; Display Manager
		      )))))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hmouse-set-bindings (key-binding-list)
  "Sets mouse keys used as Smart Keys to bindings in KEY-BINDING-LIST.
KEY-BINDING-LIST is the value returned by 'hmouse-get-bindings' prior to
Smart Key setup."
  (cond
    ;; Do nothing when running in batch mode.
    (noninteractive)
    ;;
    ;; GNU Emacs 19, XEmacs or InfoDock
    ((or hyperb:xemacs-p hyperb:emacs19-p)
     (mapcar
       (function
	 (lambda (key-and-binding)
	  (global-set-key (car key-and-binding) (cdr key-and-binding))))
       key-binding-list))
    ;;
    ;; X
    ((equal hyperb:window-system "xterm")
     (mapcar
       (function
	 (lambda (key-and-binding)
	   (define-key mouse-map (car key-and-binding) (cdr key-and-binding))))
       key-binding-list))
    ;;
    ;; NeXT
    ((equal hyperb:window-system "next")
     (mapcar
       (function
	 (lambda (key-and-binding)
	   (global-set-mouse (car key-and-binding) (cdr key-and-binding))))
       key-binding-list))
    ;;
    ;; Apollo Display Manager
    ((equal hyperb:window-system "apollo")
      (if (string-lessp emacs-version "18.58")
	  (mapcar
	    (function
	      (lambda (key-and-binding)
		(global-set-key (car key-and-binding) (cdr key-and-binding))))
	    key-binding-list)
	(mapcar
	  (function
	    (lambda (key-and-binding)
	      (define-key 'apollo-prefix (car key-and-binding)
		(cdr key-and-binding)))) 
	  key-binding-list)))))

(defun hmouse-shift-buttons (&optional arg)
  "Selects between shifted and unshifted Action and Assist mouse buttons.
With optional prefix ARG, use shifted buttons if ARG is positive or use
unshifted buttons otherwise.  If ARG is nil, shifted buttons are used and
under InfoDock the middle button also acts as an Action Key."
  (interactive "P")
  (setq hmouse-shift-flag (if arg
			      (> (prefix-numeric-value arg) 0)
			    (not (and (boundp 'infodock-version)
				      infodock-version))))
  (if hmouse-shift-flag
      ;; Action Key = shift-middle mouse key.  Assist Key = shift-right mouse
      ;; key.  Standard Hyperbole configuration.
      (load "hmouse-sh")
    ;; Action Key = middle mouse key; Assist Key = right mouse key
    ;; InfoDock actually moves the Assist Key to the shift-right mouse key so
    ;; that the right key can be used for popup menus.
    (load "hmouse-reg"))
  ;; Replace any original mouse bindings before moving Hyperbole bindings and
  ;; then force reinitialization of hmouse-previous-bindings.
  (if (and hmouse-bindings-flag hmouse-previous-bindings)
      (hmouse-set-bindings hmouse-previous-bindings))
  (setq hmouse-bindings-flag nil
	hmouse-previous-bindings nil)
  ;; Initialize Hyperbole mouse bindings.
  (hmouse-setup)
  (if (interactive-p)
      (message "%s Action and Assist mouse buttons in use."
	       (if hmouse-shift-flag "Shifted" "Unshifted"))))

(defun hmouse-toggle-bindings ()
  "Toggles between Smart Key mouse settings and their prior bindings.
Under InfoDock, the first invocation of this command will make the middle
mouse key the Paste Key instead of the Action Key."
  (interactive)
  (let ((key-binding-list (if hmouse-bindings-flag
			      hmouse-previous-bindings
			    hmouse-bindings))
	(other-list-var (if hmouse-bindings-flag
			    'hmouse-bindings
			  'hmouse-previous-bindings)))
    (if key-binding-list
	(progn
	  (set other-list-var (hmouse-get-bindings))
	  (hmouse-set-bindings key-binding-list)
	  (message "%s mouse bindings in use."
		   (if (setq hmouse-bindings-flag (not hmouse-bindings-flag))
		       "Smart Key" "Personal")))
      (error "(hmouse-toggle-bindings): Null %s." other-list-var))))

;; Disable this command since it is too easy for
;; novices to accidentally invoke.
(if (not (get 'hmouse-toggle-bindings 'enabled))
    (put 'hmouse-toggle-bindings 'disabled t))

(defun hmouse-set-point-at (set-point-arg-list)
  "Sets point to cursor position using SET-POINT-ARG-LIST and returns t.
If 'hmouse-set-point-command' is not bound to a function, this does nothing
and returns nil."
  (if (fboundp hmouse-set-point-command)
      (or (if set-point-arg-list
	      (funcall hmouse-set-point-command set-point-arg-list)
	    (funcall hmouse-set-point-command))
	  t)))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(if (fboundp 'bind-apollo-mouse-button)
    (progn
      (if (string-lessp emacs-version "18.58")
	  (defun apollo-mouse-key-and-binding (mouse-button)
	    "Returns binding for an Apollo MOUSE-BUTTON (a string) or nil if none."
	    (interactive "sMouse Button: ")
	    (let ((numeric-code (cdr (assoc mouse-button *apollo-mouse-buttons*))))
	      (if (null numeric-code)
		  (error "(hmouse-key): %s is not a valid Apollo mouse key name."
			 mouse-button))
	      (if (stringp numeric-code)
		  (setq numeric-code
			(cdr (assoc numeric-code *apollo-mouse-buttons*))))
	      (let ((key-sequence (concat "\M-*" (char-to-string numeric-code))))
		(cons key-sequence (global-key-binding key-sequence)))))
	(defun apollo-mouse-key-and-binding (mouse-button)
	  "Returns binding for an Apollo MOUSE-BUTTON (a string) or nil if none."
	  (interactive "sMouse Button: ")
	  (let ((numeric-code (cdr (assoc mouse-button *apollo-mouse-buttons*))))
	    (if (null numeric-code)
		(error "(hmouse-key): %s is not a valid Apollo mouse key name."
		       mouse-button))
	    (if (stringp numeric-code)
		(setq numeric-code
		      (cdr (assoc numeric-code *apollo-mouse-buttons*))))
	    (let ((key-sequence (char-to-string numeric-code)))
	      (cons key-sequence (lookup-key 'apollo-prefix key-sequence)))))
	)
      (defun apollo-mouse-move-point (&optional no-mark)
	"Used so that pressing the left mouse button, moving the cursor, and
releasing the left mouse button leaves the mark set to the initial position
and the point set to the final position.  Useful for easily marking regions
of text.  If the left mouse button is pressed and released at the same place,
the mark is left at the original position of the character cursor.

Returns (x y) frame coordinates of point in columns and lines."
	(interactive)
	(let* ((opoint (point))
	       (owindow (selected-window))
	       (x (- (read-char) 8))
	       (y (- (read-char) 8))
	       (edges (window-edges))
	       (window nil))
	  (while (and (not (eq window (selected-window)))
		      (or (<  y (nth 1 edges))
			  (>= y (nth 3 edges))
			  (<  x (nth 0 edges))
			  (>= x (nth 2 edges))))
	    (setq window (next-window window))
	    (setq edges (window-edges window)))
	  (if (and window (not (eq window (selected-window))))
	      (progn
		(if (and (not *apollo-mouse-move-point-allow-minibuffer-exit*)
			 (eq (selected-window) (minibuffer-window)))
		    (error "Cannot use mouse to leave minibuffer!"))
		(if (eq window (minibuffer-window))
		    (error "Cannot use mouse to enter minibuffer!"))))
	  (if window (select-window window))
	  (move-to-window-line (- y (nth 1 edges)))
	  (let* ((width-1 (1- (window-width window)))
		 (wraps (/ (current-column) width-1))
		 (prompt-length (if (eq (selected-window) (minibuffer-window))
				    (minibuffer-prompt-length)
				  0)))
	    (move-to-column (+ (- x (nth 0 edges) prompt-length)
			       (* wraps width-1))))
	  (if no-mark
	      (progn (setq window (selected-window))
		     (if (eq owindow window)
			 (if (equal opoint (point))
			     (pop-mark))
		       (select-window owindow)
		       (pop-mark)
		       (select-window window)))
	    (set-mark-command nil))
	  ;; Return (x y) coords of point in column and frame line numbers.
	  (list x y)))
      ))

(defun hmouse-save-region (&optional frame)
  "Saves any active region within the current buffer.
Under InfoDock and XEmacs, `zmacs-region' must be t; under GNU Emacs,
`transient-mark-mode' must be t or the function always nothing."
  (if (or
       ;; InfoDock and XEmacs
       (and (fboundp 'region-active-p) (region-active-p)
	    (region-exists-p))
       ;; GNU Emacs
       (and (boundp 'transient-mark-mode) transient-mark-mode mark-active))
      (setq hkey-region (buffer-substring (region-beginning) (region-end)))
    (setq hkey-region nil)))

;; InfoDock and XEmacs
(if hyperb:xemacs-p
    (add-hook 'mouse-leave-frame-hook 'hmouse-save-region)
  ;; GNU Emacs
  (add-hook 'mouse-leave-buffer-hook 'hmouse-save-region))

(defun action-key-depress (&rest args)
  (interactive)
  (require 'hsite)
  (cond (assist-key-depressed-flag
	 (or action-key-help-flag
	     (setq assist-key-help-flag t)))
	((hmouse-save-region)))
  (setq action-key-depress-prev-point (point-marker)
	action-key-depressed-flag t
	action-key-depress-args (hmouse-set-point args)
	action-key-depress-window (selected-window)
	action-key-release-args nil
	action-key-release-window nil
	action-key-release-prev-point nil))

(defun assist-key-depress (&rest args)
  (interactive)
  (require 'hsite)
  (cond (action-key-depressed-flag
	 (or assist-key-help-flag
	     (setq action-key-help-flag t)))
	((hmouse-save-region)))
  (setq assist-key-depress-prev-point (point-marker)
	assist-key-depressed-flag t
	assist-key-depress-args (hmouse-set-point args)
	assist-key-depress-window (selected-window)
	assist-key-release-args nil
	assist-key-release-window nil
	assist-key-release-prev-point nil))

(defun action-key-depress-emacs19 (event)
  (interactive "e")
  (require 'hsite)
  (action-key-depress event))

(defun assist-key-depress-emacs19 (event)
  (interactive "e")
  (require 'hsite)
  (assist-key-depress event))

(defun action-mouse-key-emacs19 (event)
  "Set point to the current mouse cursor position and execute 'action-key'.
EVENT will be passed to 'hmouse-function'."
  (interactive "e")
  (action-mouse-key (hmouse-key-release-args-emacs19 event)))

(defun assist-mouse-key-emacs19 (event)
  "Set point to the current mouse cursor position and execute 'action-key'.
EVENT will be passed to 'hmouse-function'."
  (interactive "e")
  (assist-mouse-key (hmouse-key-release-args-emacs19 event)))

(defun hmouse-key-release-args-emacs19 (event)
  (let ((ev-type-str (and (listp event) (symbol-name (car event)))))
    (if (or (and ev-type-str
		 (string-match "\\(double\\|triple\\)-mouse" ev-type-str))
	    (not (= (length event) 3)))
	event
      ;; Remove depress coordinates and send only release coordinates.
      (list (car event) (nth 2 event)))))

(defun hmouse-move-point-xemacs ()
  (condition-case ()
      (mouse-set-point current-mouse-event)
    ;; Catch "not in a window" errors, e.g. on modeline
    (error nil)))

(defun hmouse-move-point-eterm (arg-list)
  (apply 'mouse-move-point arg-list))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar hmouse-bindings nil
  "List of (key . binding) pairs for Smart Mouse Keys.")

(defvar hmouse-bindings-flag nil
  "True if Smart Key mouse bindings are in use, else nil.")

(defvar hmouse-previous-bindings nil
  "List of previous (key . binding) pairs for mouse keys used as Smart Keys.")

(provide 'hmouse-key)
