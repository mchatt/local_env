;;!emacs
;;
;; FILE:         hsys-w3.el
;; SUMMARY:      Hyperbole support for Emacs W3 World-Wide Web (WWW) browsing.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     comm, help, hypermedia
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:     7-Apr-94 at 17:17:39 by Bob Weiner
;; LAST-MOD:     14-Nov-00 at 04:38:23 by Bob Weiner
;;
;; Copyright (C) 1994, 1995, 1998  BeOpen.com
;; See the HY-COPY (Hyperbole) or BR-COPY (OO-Browser) file for license
;; information.
;;
;; This file is part of Hyperbole and the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;   This module defines an implicit button type and associated action and
;;   help types.  A press of the Action Key on a unified resource locator
;;   (URL) displays the referent for the URL.  A press of the Help Key on a
;;   URL displays a history list of previously browsed WWW documents.  Press
;;   the Action Key on any item from the history list to display it.
;;
;;   This requires the Emacs W3 World-Wide-Web browser available from:
;;     ftp://cs.indiana.edu/pub/elisp/w3/.
;;
;;   It assumes that you have set up to have w3 auto-loaded according to the
;;   setup instructions included with W3.  Specifically, `w3-fetch' should be
;;   autoloaded.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'browse-url)

;;; Requires that 'w3' or other web browser code that is called be available.

;;; ************************************************************************
;;; Public functions and types
;;; ************************************************************************

(defib www-url ()
  "Follow any non-ftp url (link) at point.
The variable, `browse-url-browser-function,' customizes the url browser that
is used."
  (let ((link-and-pos (hpath:www-at-p t)))
    ;; Skip ftp URLs which are handled elsewhere.
    (if (and link-and-pos (not (hpath:efs-at-p)))
	(progn (ibut:label-set link-and-pos)
	       (hact 'www-url (car link-and-pos))))))

(defact www-url (url)
  "Follows a link given by URL.
The variable, `browse-url-browser-function,' customizes the url browser that
is used."
  (interactive "sURL to follow: ")
  (or (stringp url)
      (error "(www-url): URL = `%s' but must be a string" url))
  (if (and window-system (fboundp browse-url-browser-function))
      (let (browse-function-name
	    browser)
	(if (symbolp browse-url-browser-function)
	    (setq browse-function-name
		  (symbol-name browse-url-browser-function)
		  browser (and (string-match
				"-\\([^-]+\\)\\'"
				browse-function-name)
			       (capitalize (substring browse-function-name
						      (match-beginning 1)
						      (match-end 1)))))
	  (setq browser "default browser"))
	(message "Sending %s to %s..." url browser)
	(funcall browse-url-browser-function url)
	(message "Sending %s to %s...done" url browser))
    (w3-fetch url)))

(defun www-url:help (&optional but)
  "Displays history list of www nodes previously visited with the W3 browser."
  (interactive)
  (if (fboundp 'w3-show-history-list)
      (hact 'w3-show-history-list)
    (hact 'error "(www-url:help): W3 must be loaded to display WWW history")))

;;;###autoload
(defun www-url-expand-file-name (path &optional dir)
  "Expand PATH in DIR.  Return http urls unchanged."
  (if (listp path)
      (setq dir  (car (cdr path))
	    path (car path)))
  (cond ((string-match "\\`www\\.\\|\\`https?:" path)
	 path)
	(t (require 'hpath)
	   (or (hpath:efs-p path) path))))

;;;###autoload
(defun www-url-find-file-noselect (path &rest args)
  "Find PATH without selecting its buffer.  Handle http urls."
  (if (listp path)
      (setq args (cdr path)
	    path (car path)))
  (let ((inhibit-file-name-handlers
	 (append '(dired-handler-fn efs-file-handler-function)
		 (and (eq inhibit-file-name-operation 'find-file-noselect)
		      inhibit-file-name-handlers)))
	(inhibit-file-name-operation 'find-file-noselect))
    (if (string-match "\\`www\\.\\|\\`https?:" path)
	(progn (require 'hsite)
	       ;; Display url.
	       (hact 'www-url path)
	       ;; return same buffer
	       (current-buffer))
      (apply 'find-file-noselect path args))))

(provide 'hsys-w3)
