;;!emacs
;;
;; FILE:         c++-browse.el
;; SUMMARY:      C++ source code browser.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     c, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:    12-Dec-89
;; LAST-MOD:     10-May-01 at 12:49:26 by Bob Weiner
;;
;; Copyright (C) 1989-1995, 1997  BeOpen.com
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;    Use `c++-browse' to invoke the C++ OO-Browser.  Prefix arg prompts for
;;    name of Environment file.
;;
;; DESCRIP-END.

;; ************************************************************************
;; Other required Elisp libraries
;; ************************************************************************

(mapcar 'require '(br-start br br-c++-ft))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun c++-browse (&optional env-file no-ui)
  "Invoke the C++ OO-Browser.
This allows browsing through C++ library and system class hierarchies.  With
an optional non-nil prefix argument ENV-FILE, prompt for Environment file to
use.  Alternatively, a string value of ENV-FILE is used as the Environment
file name.  See also the file \"br-help\"."
  (interactive "P")
  (let ((same-lang (equal br-lang-prefix c++-lang-prefix))
	(load-succeeded t)
	same-env)
    (if same-lang
	nil
      ;; Save other language Environment in memory
      (if br-lang-prefix (br-env-copy nil))
      (setq br-lang-prefix c++-lang-prefix
	    *br-save-wconfig* nil))
    ;; `same-env' non-nil means the new Env is the previous Env or the most
    ;; recent previous Env of the same language as the new Env
    (setq same-env (or (equal c++-env-file env-file)
		       (and (null env-file)
			    (or c++-lib-search-dirs c++-sys-search-dirs)
			    t)))
    (cond
     (same-env
      ;; If we just switched languages, restore the cached data for the new
      ;; Environment.
      (if same-lang nil (br-env-copy t))
      ;; Environment may appear to be the same but its loading may have
      ;; been interrupted, so ensure all variables are initialized properly.
      (c++-browse-setup env-file)
      (if (or (null br-paths-htable) (equal br-paths-htable br-empty-htable))
	  (setq load-succeeded
		(br-env-try-load (or env-file br-env-file) br-env-file))))
     ;;
     ;; Create default Environment file specification if needed and none
     ;; exists.
     ;;
     (t (or env-file (file-exists-p c++-env-file)
	    (br-env-create c++-env-file c++-lang-prefix))
	(or env-file (setq env-file c++-env-file))
	;;
	;; Start browsing a new Environment.
	;;
	(c++-browse-setup env-file)
	(setq load-succeeded (br-env-init env-file same-lang nil))
	(if load-succeeded
	    (setq *br-save-wconfig* nil
		  c++-env-file br-env-file
		  c++-env-name br-env-name
		  c++-sys-search-dirs br-sys-search-dirs
		  c++-lib-search-dirs br-lib-search-dirs))))
    (cond (load-succeeded
	   (if no-ui
	       nil
	     (br-browse)
	     (or (and same-lang same-env) (br-refresh))))
	  (no-ui nil)
	  (t (message "(c++-browse): You must build the Environment to browse it.")))))

;; Don't filter Environment classes when listed.
(defalias 'c++-class-list-filter 'br-class-list-identity)

(defun c++-mode-setup ()
  "Load best available C++ major mode and set `br-lang-mode' to the function that invokes it."
  (defalias 'br-lang-mode
	(cond ((or (featurep 'cc-mode) (featurep 'c++-mode))
	       'c++-mode)
	      ((load "cc-mode" 'missing-ok 'nomessage)
	       (provide 'c++-mode))
	      ((load "c++-mode" 'missing-ok 'nomessage)
	       (provide 'c++-mode))
	      ((featurep 'c-mode) 'c-mode)
	      ((load "c-mode" nil 'nomessage)
	       (provide 'c-mode))))
  (condition-case ()
      (progn (require 'cc-mode)
	     (c-initialize-cc-mode))
    (error nil)))

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(defun c++-browse-setup (env-file)
  "Setup language-dependent functions for OO-Browser."
  (br-setup-functions)
  (c++-mode-setup)
  (br-setup-constants env-file)
  ;; Setup to add default classes to system class table after building it.
  ;; This must come after br-setup-constants call since it clears these
  ;; hooks.
  (if (fboundp 'add-hook)
      (add-hook 'br-after-build-sys-hook 'c++-add-default-classes)
    (setq br-after-build-sys-hook '(c++-add-default-classes))))

(provide 'c++-browse)
