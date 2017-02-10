;;!emacs
;;
;; FILE:         java-brows.el
;; SUMMARY:      Java source code browser.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     c, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:    01-Aug-95
;; LAST-MOD:     10-May-01 at 12:53:09 by Bob Weiner
;;
;; Copyright (C) 1995, 1996, 1997, 1998  BeOpen.com
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;    Use `java-browse' to invoke the java OO-Browser.  Prefix arg prompts for
;;    name of Environment file.
;;
;; DESCRIP-END.

;; ************************************************************************
;; Other required Elisp libraries
;; ************************************************************************

(mapcar 'require '(br-start br br-java-ft))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun java-browse (&optional env-file no-ui)
  "Invoke the Java OO-Browser.
This allows browsing through Java library and system class hierarchies.  With
an optional non-nil prefix argument ENV-FILE, prompt for Environment file to
use.  Alternatively, a string value of ENV-FILE is used as the Environment
file name.  See also the file \"br-help\"."
  (interactive "P")
  (let ((same-lang (equal br-lang-prefix java-lang-prefix))
	(load-succeeded t)
	same-env)
    (if same-lang
	nil
      ;; Save other language Environment in memory
      (if br-lang-prefix (br-env-copy nil))
      (setq br-lang-prefix java-lang-prefix
	    *br-save-wconfig* nil))
    ;; `same-env' non-nil means the new Env is the previous Env or the most
    ;; recent previous Env of the same language as the new Env
    (setq same-env (or (equal java-env-file env-file)
		       (and (null env-file)
			    (or java-lib-search-dirs java-sys-search-dirs))))
    (cond
     (same-env
      ;; If we just switched languages, restore the cached data for the new
      ;; Environment.
      (if same-lang nil (br-env-copy t))
      ;; Environment may appear to be the same but its loading may have
      ;; been interrupted, so ensure all variables are initialized properly.
      (java-browse-setup env-file)
      (if (or (null br-paths-htable) (equal br-paths-htable br-empty-htable))
	  (setq load-succeeded
		(br-env-try-load (or env-file br-env-file) br-env-file))))
     ;;
     ;; Create default Environment file specification if needed and none
     ;; exists.
     ;;
     (t (or env-file (file-exists-p java-env-file)
	    (br-env-create java-env-file java-lang-prefix))
	(or env-file (setq env-file java-env-file))
	;;
	;; Start browsing a new Environment.
	;;
	(java-browse-setup env-file)
	(setq load-succeeded (br-env-init env-file same-lang nil))
	(if load-succeeded
	    (setq *br-save-wconfig* nil
		  java-env-file br-env-file
		  java-env-name br-env-name
		  java-sys-search-dirs br-sys-search-dirs
		  java-lib-search-dirs br-lib-search-dirs))))
    (cond (load-succeeded
	   (if no-ui
	       nil
	     (br-browse)
	     (or (and same-lang same-env) (br-refresh))))
	  (no-ui nil)
	  (t (message "(java-browse): You must build the Environment to browse it.")))))

(defun java-class-list-filter (class-list top-only-flag)
  "Return CLASS-LIST sans any interface entries.
Used when Environment classes are listed in the initial listing buffer."
  (cond
   (top-only-flag
    (let (parents)
      (delq
       nil
       (br-flatten
	(mapcar
	 (function
	  (lambda (class)
	    (cond
	     ;; interface / abstract class
	     ((string-match "\\`<" class)
	      (if br-protocols-with-classes-flag
		  (if (br-get-parents class)
		      nil
		    class)
		(java-class-list-filter
		 (br-get-children class) t)))
	     ;;
	     ;; Regular class; because of recursion
	     ;; from the above clause, we must ensure
	     ;; that this class has no concrete parents
	     (t (setq parents (br-get-parents class))
		(if (or (null parents)
			(not (delq nil
				   (mapcar
				    (function
				     (lambda (parent)
				       (not (string-match "\\`<" parent))))
				    parents))))
		    class)))))
	 class-list)))))
    (br-protocols-with-classes-flag class-list)
    (t (delq nil (mapcar (function
			  (lambda (class)
			    (if (string-match "\\`<" class)
				nil
			      class)))
			 class-list)))))

(defun java-lang-mode ()
  "Invoke normal-mode for programming language buffers and java-mode for any others."
  (normal-mode)
  (if (not (memq major-mode '(java-mode c++-mode c-mode)))
      (java-mode)))

(defun java-mode-setup ()
  "Load best available java major mode and set `br-lang-mode' to the function that invokes it."
  (defalias 'br-lang-mode 'java-lang-mode)
  (condition-case ()
      (progn (require 'cc-mode)
	     (c-initialize-cc-mode))
    (error nil)))

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(defun java-browse-setup (env-file)
  "Setup language-dependent functions for OO-Browser."
  (br-setup-functions)
  (java-mode-setup)
  (br-setup-constants env-file)
  ;; Setup to add default classes to system class table after building it.
  ;; This must come after br-setup-constants call since it clears these
  ;; hooks.
  (if (fboundp 'add-hook)
      (add-hook 'br-after-build-sys-hook 'java-add-default-classes)
    (setq br-after-build-sys-hook '(java-add-default-classes))))

(provide 'java-brows)
