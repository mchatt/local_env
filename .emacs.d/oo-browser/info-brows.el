;;!emacs
;;
;; FILE:         info-brows.el
;; SUMMARY:      Support routines for Info inheritance browsing and error parsing.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     docs, help, hypermedia
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:     7-Dec-89
;; LAST-MOD:     10-May-01 at 12:52:49 by Bob Weiner
;;
;; Copyright (C) 1989-1995, 1997  BeOpen.com
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;    Use `info-browse' to invoke the Info OO-Browser.  Prefix arg prompts for
;;    name of Environment file.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(mapcar 'require '(info br-start br br-info))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun info-browse (&optional env-file no-ui)
  "Invoke the Info OO-Browser.
This allows browsing through Info library and system class hierarchies.  With
an optional non-nil prefix argument ENV-FILE, prompt for Environment file to
use.  Alternatively, a string value of ENV-FILE is used as the Environment
file name.  See also the file \"br-help\"."
  (interactive "P")
  (let ((same-lang (equal br-lang-prefix info-lang-prefix))
	(load-succeeded t)
	same-env)
    (if same-lang
	nil
      ;; Save other language Environment in memory
      (if br-lang-prefix (br-env-copy nil))
      (setq br-lang-prefix info-lang-prefix
	    *br-save-wconfig* nil))
    ;; `same-env' non-nil means the new Env is the previous Env or the most
    ;; recent previous Env of the same language as the new Env
    (setq same-env (or (equal info-env-file env-file)
		       (and (null env-file)
			    (or info-lib-search-dirs info-sys-search-dirs))))
    (cond
     (same-env
      ;; If we just switched languages, restore the cached data for the new
      ;; Environment.
      (if same-lang nil (br-env-copy t))
      ;; Environment may appear to be the same but its loading may have
      ;; been interrupted, so ensure all variables are initialized properly.
      (info-browse-setup env-file)
      (if (or (null br-paths-htable) (equal br-paths-htable br-empty-htable))
	  (setq load-succeeded
		(br-env-try-load (or env-file br-env-file) br-env-file))))

     ;;
     ;; Create default Environment file specification if needed and none
     ;; exists.
     ;;
     (t (or env-file (file-exists-p info-env-file)
	    (br-env-create info-env-file info-lang-prefix))
	(or env-file (setq env-file info-env-file))
	;;
	;; Start browsing a new Environment.
	;;
	(info-browse-setup env-file)
	(setq load-succeeded (br-env-init env-file same-lang nil))
	(if load-succeeded
	    (setq *br-save-wconfig* nil
		  info-env-file br-env-file
		  info-env-name br-env-name
		  info-sys-search-dirs br-sys-search-dirs
		  info-lib-search-dirs br-lib-search-dirs))))
    (cond (load-succeeded
	   (if no-ui
	       nil
	     (br-browse)
	     (or (and same-lang same-env) (br-refresh))))
	  (no-ui nil)
	  (t (message "(info-browse): You must build the Environment to browse it.")))))

;; Don't filter Environment classes when listed.
(defalias 'info-class-list-filter 'br-class-list-identity)

(defun info-class-definition-regexp (class)
  "Return regexp to uniquely match the definition of CLASS name."
  (concat info-class-name-before (regexp-quote class)
	  info-class-name-after))

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(defun info-find-nd-name ()
  (let ((end))
    (save-excursion
      (skip-chars-forward info-identifier-chars)
      (setq end (point))
      (skip-chars-backward info-identifier-chars)
      (skip-chars-forward " \n\r")
      (br-buffer-substring (point) end))))

(defun info-browse-setup (env-file)
  "Setup language-dependent functions for OO-Browser."
  (br-setup-functions)
  (defalias 'br-lang-mode 'Info-mode)
  (defalias 'br-find-class-name 'info-find-nd-name)
  (br-setup-constants env-file))

(provide 'info-browse)
