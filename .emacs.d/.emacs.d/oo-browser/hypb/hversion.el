;;!emacs
;;
;; LCD-ENTRY:    Hyperbole|Bob Weiner|info@beopen.com|Everyday Info Manager|18-Jul-1999|04.18|http://www.beopen.com
;;
;; FILE:         hversion.el
;; SUMMARY:      Hyperbole version, system and load path information.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     hypermedia
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:     1-Jan-94
;; LAST-MOD:     18-Jul-99 at 15:45:27 by Bob Weiner
;;
;; Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999  BeOpen.com
;; See the HY-COPY (Hyperbole) or BR-COPY (OO-Browser) file for license
;; information.
;;
;; This file is part of Hyperbole and the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defconst hyperb:version "04.18" "Hyperbole revision number.")

;;; Support button highlighting and flashing under XEmacs.
;;;
;;;###autoload
(defvar hyperb:xemacs-p
  (let ((case-fold-search t))
    (if (string-match "XEmacs" emacs-version)
	emacs-version))
  "Version string under XEmacs or nil")

;;; Support mouse handling under GNU Emacs V19 or higher.
;;;
;;;###autoload
(defvar hyperb:emacs19-p
  (and (not hyperb:xemacs-p)
       ;; Version 19 and above.
       (string-lessp "19" emacs-version)
       emacs-version)
  "Version string under GNU Emacs 19 or higher, or nil")

;;; Koutlines work only with specific versions of Emacs 19 and XEmacs.
;;;###autoload
(defconst hyperb:kotl-p
  (if hyperb:xemacs-p
      ;; Only works for XEmacs 19.9 and above.
      (or (string-match "^19\\.9 \\|^19\\.[1-9][0-9]" emacs-version)
	  ;; Version 20 and above.
	  (string-lessp "20" emacs-version))
    hyperb:emacs19-p)
  "Non-nil iff this Emacs version supports the Hyperbole outliner.")

;;; Account for what rain all year round and working for two ex-Harvard guys
;;; will do to programmers.
;;;###autoload
(defvar hyperb:microcruft-os-p
  (memq system-type '(ms-windows windows-nt ms-dos win32))
  "T iff Hyperbole is running under a Microcruft OS.")

;;;###autoload
(defvar hyperb:mouse-buttons
  (if (or (and hyperb:microcruft-os-p (not (eq window-system 'x)))
	  (and hyperb:emacs19-p (memq window-system '(ns dps))))
      2 3)
  "*Number of live buttons available on the mouse.
Override this if the system-computed default is incorrect for your specific mouse.")

(defun sm-window-sys-term ()
  "Returns the first part of the term-type if running under a window system, else nil.
Where a part in the term-type is delimited by a `-' or  an `_'."
  (let* ((display-type (if (fboundp 'device-type) (device-type) window-system))
	 (term (cond ((memq display-type '(x mswindows win32 w32 ns dps pm))
		      ;; X11, NEXTSTEP (DPS), or OS/2 Presentation Manager (PM)
		      (cond (hyperb:emacs19-p "emacs19")
			    (hyperb:xemacs-p  "xemacs")
			    (t                "xterm")))
		     ((or (featurep 'eterm-fns)
			  (equal (getenv "TERM") "NeXT")
			  (equal (getenv "TERM") "eterm"))
		      ;; NEXTSTEP add-on support to Emacs
		      "next")
		     ((or display-type (featurep 'apollo))
		      (getenv "TERM")))))
    (and term
	 (substring term 0 (string-match "[-_]" term)))))

(defconst hyperb:window-system (sm-window-sys-term)
  "String name for window system or term type under which Hyperbole is running.
If nil, no window system or mouse support is available.")

;; It is possible for this file to be loaded before the first frame is
;; initialized in which case `hyperb:window-system' will have the wrong
;; value.  This hook re-initializes it to the proper value at the end of the
;; startup sequence.
(add-hook 'after-init-hook
	  (function
	   (lambda ()
	     (setq hyperb:window-system (sm-window-sys-term)))))

;;; ************************************************************************
;;; Public functions to dynamically compute Hyperbole directory.
;;; ************************************************************************

(defvar hyperb:automount-prefixes
  (if (and (boundp 'automount-dir-prefix) (stringp automount-dir-prefix))
      automount-dir-prefix
    "^/tmp_mnt/"
    "*Regexp to match any automounter prefix in a pathname."))

(defun hyperb:stack-frame (function-list &optional debug-flag)
  "Return the nearest Emacs Lisp stack frame which called any function symbol from FUNCTION-LIST or nil if no match.
If FUNCTION-LIST contains 'load, 'autoload or 'require, detect
autoloads not visible within the Lisp level stack frames.

With optional DEBUG-FLAG non-nil, if no matching frame is found, return list
of stack frames (from innermost to outermost)."
  (let ((count 0)
	(frame-list)
	(load-flag (or (memq 'load function-list)
		       (memq 'autoload function-list)
		       (memq 'require function-list)))
	fsymbol
	fbody
	frame)
    (or (catch 'hyperb:stack-frame
	  (while (setq frame (backtrace-frame count))
	    (if debug-flag (setq frame-list (cons frame frame-list)))
	    (setq count (1+ count)
		  fsymbol (nth 1 frame))
	    (and (eq fsymbol 'command-execute)
		 (not (memq 'command-execute function-list))
		 ;; Use command being executed instead because it might not
		 ;; show up in the stack anywhere else, e.g. if it is an
		 ;; autoload under Emacs 19.
		 (setq fsymbol (nth 2 frame)))
	    (cond ((and load-flag (symbolp fsymbol)
			(fboundp fsymbol)
			(listp (setq fbody (symbol-function fsymbol)))
			(eq (car fbody) 'autoload))
		   (setq frame (list (car frame) 'load
				     (car (cdr fbody))
				     nil noninteractive nil))
		   (throw 'hyperb:stack-frame frame))
		  ((memq fsymbol function-list)
		   (throw 'hyperb:stack-frame frame))))
	  nil)
	(if debug-flag (nreverse frame-list)))))

(if (fboundp 'locate-file)
    nil
(defun locate-file (file dir-list &optional suffix-string unused)
  "Search for FILE in DIR-LIST.
If optional SUFFIX-STRING is provided, allow file to be followed by one of the
colon separated suffixes."
  (let ((suffix-list))
    (cond ((null suffix-string))
	  ((stringp suffix-string)
	   (let ((start 0)
		 (len  (length suffix-string)))
	     (while (and (< start len)
			 (string-match "[^:]+" suffix-string start))
	       (setq suffix-list
		     (cons (substring suffix-string
				      (match-beginning 0)
				      (match-end 0))
			   suffix-list)
		     start (1+ (match-end 0))))
	     (setq suffix-list (nconc (nreverse suffix-list) '("")))))
	  (t (error "(locate-file): Invalid third arg, `%s', use a colon separated string of file suffixes"
		    suffix-string)))
    ;;
    (if (and (file-name-absolute-p file) (file-readable-p file))
	file;; file exists without suffix addition, so return it
      (if (file-name-absolute-p file) (setq dir-list '(nil)))
      (if (equal file "") (error "(locate-file): Empty file argument"))
      (let (suffixes pathname)
	;; Search dir-list for a matching, readable file.
	(catch 'found
	  (while dir-list
	    (setq suffixes suffix-list)
	    (if suffixes
		(while suffixes
		  (setq pathname (expand-file-name
				  (concat file (car suffixes))
				  (car dir-list)))
		  (if (file-readable-p pathname)
		      (throw 'found pathname))
		  (setq suffixes (cdr suffixes)))
	      (setq pathname (expand-file-name file (car dir-list)))
	      (if (file-readable-p pathname)
		  (throw 'found pathname)))
	    (setq dir-list (cdr dir-list)))))))))

;;; ************************************************************************
;;; Public functions used by pulldown and popup menus
;;; ************************************************************************

(if (not (fboundp 'id-browse-file))
(defalias 'id-browse-file 'find-file-read-only))

(if (not (fboundp 'id-info))
(defun id-info (node)
  (if (br-in-browser) (br-to-view-window))
  ;; Force execution of Info-mode-hook which adds the OO-Browser man
  ;; directory to Info-directory-list.
  (info)
  (Info-goto-node node)))

(if (not (fboundp 'id-tool-quit))
(defalias 'id-tool-quit 'eval))

(if (not (fboundp 'id-tool-invoke))
(defun id-tool-invoke (sexp)
  (if (commandp sexp)
      (call-interactively sexp)
    (funcall sexp))))

(provide 'hversion)
