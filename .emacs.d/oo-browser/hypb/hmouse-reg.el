;;!emacs
;;
;; FILE:         hmouse-reg.el
;; SUMMARY:      System-dependent Smart Mouse Key bindings (no shift key).
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     hypermedia, mouse
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:     3-Sep-91 at 21:40:58
;; LAST-MOD:     13-Jun-99 at 01:17:18 by Bob Weiner
;;
;; Copyright (C) 1991-1998  BeOpen.com
;; See the HY-COPY (Hyperbole) or BR-COPY (OO-Browser) file for license
;; information.
;;
;; This file is part of Hyperbole and the OO-Browser.

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hmouse-get-bindings ()
  "Returns list of bindings for mouse keys prior to their use as Smart Keys."
  ;; Do nothing when running in batch mode.
  (if noninteractive
      nil
    (eval
     (cdr (assoc
	   ;; Get mouse bindings under Emacs 19 or XEmacs, even if not under a
	   ;; window system since it can have frames on ttys and windowed
	   ;; displays at the same time.
	   (or (and hyperb:xemacs-p "xemacs")
	       (and hyperb:emacs19-p "emacs19")
	       hyperb:window-system)
	   '(("emacs19" .
	      (mapcar (function
		       (lambda (key) (cons key (lookup-key global-map key))))
		      (if (memq window-system '(ns dps))
			  ;; NEXTSTEP offers only 2 mouse buttons which we use
			  ;; as the Smart Keys.  We move the mouse-set-point
			  ;; command to shift-left.
			  '([down-mouse-1] [mouse-1] [down-mouse-2] [mouse-2]
			    [double-mouse-1] [triple-mouse-1]
			    [double-mouse-2] [triple-mouse-2]
			    [vertical-line down-mouse-1] [vertical-line mouse-1]
			    [vertical-line down-mouse-2] [vertical-line mouse-2]
			    [mode-line down-mouse-1] [mode-line mouse-1]
			    [mode-line down-mouse-2] [mode-line mouse-2]
			    [S-mouse-1]
			    )
			;; X
			'([down-mouse-2] [mouse-2] [down-mouse-3] [mouse-3]
			  [double-mouse-2] [triple-mouse-2]
			  [double-mouse-3] [triple-mouse-3]
			  [vertical-line down-mouse-2] [vertical-line mouse-2]
			  [vertical-line down-mouse-3] [vertical-line mouse-3]
			  [mode-line down-mouse-2] [mode-line mouse-2]
			  [mode-line down-mouse-3] [mode-line mouse-3]
			  ))))
	     ("xemacs" .
	      (nconc
	       (mapcar (function
			(lambda (key)
			  (cons key (lookup-key global-map key))))
		       '([button2] [button2up] [button3] [button3up]))
	       (if (boundp 'mode-line-map)
		   (mapcar (function
			    (lambda (key)
			      (cons key (lookup-key mode-line-map key))))
			   '([button3] [button3up])))))
	     ("xterm" .
	      (mapcar (function
		       (lambda (key) (cons key (lookup-key mouse-map key))))
		      (list x-button-middle x-button-middle-up
			    x-button-right  x-button-right-up)))
	     ("next" .
	      (mapcar (function
		       (lambda (key)
			 (cons key (mousemap-get
				    (mouse-list-to-mouse-code key)
				    current-global-mousemap))))
		      (apply 'nconc
			     (mapcar (function
				      (lambda (region)
					(mapcar (function
						 (lambda (key)
						   (cons region key)))
						'((left) (up left) (shift left)
						  (right) (up right)
						  ))))
				     '(text scrollbar modeline minibuffer)))
		      ))
	     ("apollo" .
	      (mapcar (function
		       (lambda (key-str) (apollo-mouse-key-and-binding
					  key-str)))
		      '("M2D" "M2U" "M3D" "M3U")))
	     ))))))

(defun hmouse-setup ()
  "Binds mouse keys for use as Smart Keys."
  (interactive)
  ;; Do nothing when running in batch mode.
  (if noninteractive
      nil
    (or hmouse-bindings-flag hmouse-previous-bindings
	(setq hmouse-previous-bindings (hmouse-get-bindings)))
    ;; Ensure Gillespie's Info mouse support is off since
    ;; Hyperbole handles that.
    (setq Info-mouse-support nil)
    ;;
    (cond;; GNU Emacs 19
     (hyperb:emacs19-p
      (setq hmouse-set-point-command 'mouse-set-point)
      ;; Get rid of Info-mode [mouse-2] binding since Hyperbole performs
      ;; a superset of what it does.
      (add-hook 'Info-mode-hook
		(function (lambda () (define-key Info-mode-map [mouse-2] nil))))
      ;;
      (if (memq window-system '(ns dps))
	  ;; NEXTSTEP offers only 2 mouse buttons which we use
	  ;; as the Smart Keys.  We move the mouse-set-point
	  ;; command to shift-left.
	  (progn
	    (global-set-key [S-down-mouse-1]      'mouse-drag-region)
	    (global-set-key [S-mouse-1]           'mouse-set-point)
	    (global-set-key [down-mouse-1]        'action-key-depress-emacs19)
	    (global-set-key [mouse-1]             'action-mouse-key-emacs19)
	    (global-set-key [double-mouse-1]      'action-mouse-key-emacs19)
	    (global-set-key [triple-mouse-1]      'action-mouse-key-emacs19)
	    (global-set-key [down-mouse-2]        'assist-key-depress-emacs19)
	    (global-set-key [mouse-2]             'assist-mouse-key-emacs19)
	    (global-set-key [double-mouse-2]      'assist-mouse-key-emacs19)
	    (global-set-key [triple-mouse-2]      'assist-mouse-key-emacs19)
	    (global-set-key [vertical-line down-mouse-1] 'action-key-depress-emacs19)
	    (global-set-key [vertical-line mouse-1] 'action-mouse-key-emacs19)
	    (global-set-key [vertical-line down-mouse-2] 'assist-key-depress-emacs19)
	    (global-set-key [vertical-line mouse-2] 'assist-mouse-key-emacs19)
	    (global-set-key [mode-line down-mouse-2] 'action-key-depress-emacs19)
	    (global-set-key [mode-line mouse-2]      'action-mouse-key-emacs19)
	    (global-set-key [mode-line down-mouse-3] 'assist-key-depress-emacs19)
	    (global-set-key [mode-line mouse-3]   'assist-mouse-key-emacs19))
	;; X
	(global-set-key [down-mouse-2]           'action-key-depress-emacs19)
	(global-set-key [mouse-2]                'action-mouse-key-emacs19)
	(global-set-key [double-mouse-2]         'action-mouse-key-emacs19)
	(global-set-key [triple-mouse-2]         'action-mouse-key-emacs19)
	(global-set-key [down-mouse-3]           'assist-key-depress-emacs19)
	(global-set-key [mouse-3]                'assist-mouse-key-emacs19)
	(global-set-key [double-mouse-3]         'assist-mouse-key-emacs19)
	(global-set-key [triple-mouse-3]         'assist-mouse-key-emacs19)
	(global-set-key [vertical-line down-mouse-2] 'action-key-depress-emacs19)
	(global-set-key [vertical-line mouse-2]      'action-mouse-key-emacs19)
	(global-set-key [vertical-line down-mouse-3] 'assist-key-depress-emacs19)
	(global-set-key [vertical-line mouse-3]      'assist-mouse-key-emacs19)
	(global-set-key [mode-line down-mouse-2] 'action-key-depress-emacs19)
	(global-set-key [mode-line mouse-2]      'action-mouse-key-emacs19)
	(global-set-key [mode-line down-mouse-3] 'assist-key-depress-emacs19)
	(global-set-key [mode-line mouse-3]      'assist-mouse-key-emacs19)))
     ;;
     ;; XEmacs
     (hyperb:xemacs-p
      ;; Set mouse bindings under XEmacs, even if not under a window
      ;; system since it can have frames on ttys and windowed displays at
      ;; the same time.
      (setq hmouse-set-point-command 'hmouse-move-point-xemacs)
      ;; Get rid of Info-mode buttons 2 and 3 bindings since Hyperbole
      ;; handles things in Info.
      (add-hook 'Info-mode-hook
		(function (lambda ()
			    (define-key Info-mode-map 'button2 nil))))
      ;;
      (global-set-key 'button2     'action-key-depress)
      (global-set-key 'button2up   'action-mouse-key)
      (if (fboundp 'infodock-set-mouse-bindings)
	  (infodock-set-mouse-bindings)
	(let ((unbind-but3
	       (function (lambda ()
			   (define-key Info-mode-map 'button3 nil)))))
	  (if (and (boundp 'Info-mode-map) (keymapp Info-mode-map))
	      (funcall unbind-but3)
	    (add-hook 'Info-mode-hook unbind-but3)))
	(if (boundp 'mode-line-map)
	    (progn (define-key mode-line-map 'button3   'assist-key-depress)
		   (define-key mode-line-map 'button3up 'assist-mouse-key)))
	(global-set-key 'button3     'assist-key-depress)
	(global-set-key 'button3up   'assist-mouse-key)))
     ;;
     ;; X
     ((equal hyperb:window-system "xterm")
      (setq hmouse-set-point-command 'x-mouse-set-point)
      (define-key mouse-map x-button-middle 'action-key-depress)
      (define-key mouse-map x-button-middle-up 'action-mouse-key)
      (define-key mouse-map x-button-right 'assist-key-depress)
      (define-key mouse-map x-button-right-up 'assist-mouse-key)
      ;; Use these instead of the above for a true META-BUTTON binding.
      ;; (define-key mouse-map x-button-m-middle 'assist-key-depress)
      ;; (define-key mouse-map x-button-m-middle-up 'assist-mouse-key)
      )
     ;;
     ;; NeXT
     ((equal hyperb:window-system "next")
      (setq hmouse-set-point-command 'hmouse-move-point-eterm)
      ;; Use shift-left button to set point.
      ;; Use left button instead of non-existent middle as Smart Key.
      (mapcar
       (function
	(lambda (region)
	  (global-set-mouse (cons region '(shift left)) 'mouse-move-point)
	  (global-set-mouse (cons region '(left))       'action-key-depress)
	  (global-set-mouse (cons region '(up left))    'action-mouse-key)
	  (global-set-mouse (cons region '(right))      'assist-key-depress)
	  (global-set-mouse (cons region '(up right))   'assist-mouse-key)
	  ;; Use these instead of the above for a true META-BUTTON binding.
	  ;; (global-set-mouse (cons region '(meta    right)) 'assist-key-depress)
	  ;; (global-set-mouse (cons region '(meta up right)) 'assist-mouse-key)
	  ))
       '(text scrollbar modeline minibuffer))
      )
     ;;
     ;; Apollo DM
     ((equal hyperb:window-system "apollo")
      (setq hmouse-set-point-command 'apollo-mouse-move-point)
      (bind-apollo-mouse-button "M2D" 'action-key-depress)
      (bind-apollo-mouse-button "M2U" 'action-mouse-key)
      (bind-apollo-mouse-button "M3D" 'assist-key-depress)
      (bind-apollo-mouse-button "M3U" 'assist-mouse-key)
      ;; Use these instead of the above for a true META-BUTTON binding.
      ;; (bind-apollo-mouse-button "M2U" 'action-mouse-key
      ;;  'assist-mouse-key)
      ;; (bind-apollo-mouse-button "M2D" 'action-key-depress 'assist-key-depress)
      ))
    (setq hmouse-bindings (hmouse-get-bindings)
	  hmouse-bindings-flag t)))
