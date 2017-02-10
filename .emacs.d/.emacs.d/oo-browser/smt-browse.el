;;!emacs
;;
;; FILE:         smt-browse.el
;; SUMMARY:      Smalltalk source code browser.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:    26-Jul-90
;; LAST-MOD:     10-May-01 at 12:54:27 by Bob Weiner
;;
;; Copyright (C) 1990-1995, 1997  BeOpen.com
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;    Use `smt-browse' to invoke the Smalltalk OO-Browser.  Prefix arg
;;    prompts for name of Environment file.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(mapcar 'require '(br-start br br-smt))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun smt-browse (&optional env-file no-ui)
  "Invoke the Smalltalk OO-Browser.
This allows browsing through Smalltalk library and system class hierarchies.
With an optional non-nil prefix argument ENV-FILE, prompt for Environment
file to use.  Alternatively, a string value of ENV-FILE is used as the
Environment file name.  See also the file \"br-help\"."
  (interactive "P")
  (let ((same-lang (equal br-lang-prefix smt-lang-prefix))
	(load-succeeded t)
	same-env)
    (if same-lang
	nil
      ;; Save other language Environment in memory
      (if br-lang-prefix (br-env-copy nil))
      (setq br-lang-prefix smt-lang-prefix
	    *br-save-wconfig* nil))
    ;; `same-env' non-nil means the new Env is the previous Env or the most
    ;; recent previous Env of the same language as the new Env
    (setq same-env (or (equal smt-env-file env-file)
		       (and (null env-file)
			    (or smt-lib-search-dirs smt-sys-search-dirs))))
    (cond
     (same-env
      ;; If we just switched languages, restore the cached data for the new
      ;; Environment.
      (if same-lang nil (br-env-copy t))
      ;; Environment may appear to be the same but its loading may have
      ;; been interrupted, so ensure all variables are initialized properly.
      (smt-browse-setup env-file)
      (if (or (null br-paths-htable) (equal br-paths-htable br-empty-htable))
	  (setq load-succeeded
		(br-env-try-load (or env-file br-env-file) br-env-file))))
     ;;
     ;; Create default Environment file specification if needed and none
     ;; exists.
     ;;
     (t (or env-file (file-exists-p smt-env-file)
	    (br-env-create smt-env-file smt-lang-prefix))
	(or env-file (setq env-file smt-env-file))
	;;
	;; Start browsing a new Environment.
	;;
	(smt-browse-setup env-file)
	(setq load-succeeded (br-env-init env-file same-lang nil))
	(if load-succeeded
	    (setq *br-save-wconfig* nil
		  smt-env-file br-env-file
		  smt-env-name br-env-name
		  smt-sys-search-dirs br-sys-search-dirs
		  smt-lib-search-dirs br-lib-search-dirs))))
    (cond (load-succeeded
	   (if no-ui
	       nil
	     (br-browse)
	     (or (and same-lang same-env) (br-refresh))))
	  (no-ui nil)
	  (t (message "(smt-browse): You must build the Environment to browse it.")))))

;; Don't filter Environment classes when listed.
(defalias 'smt-class-list-filter 'br-class-list-identity)

(defun smt-class-definition-regexp (class)
  "Return regexp to uniquely match the definition of CLASS name."
  (concat smt-class-name-before (regexp-quote class)
	  smt-class-name-after))

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(defun smt-browse-setup (env-file)
  "Setup language-dependent functions for OO-Browser."
  (br-setup-functions)
  (defalias 'br-lang-mode
	(cond ((featurep 'smalltalk-mode) 'smalltalk-mode)
	      ((load "st" 'missing-ok 'nomessage)
	       (provide 'smalltalk-mode))
	      (t 'fundamental-mode)))
  (br-setup-constants env-file))

(provide 'smt-browse)
