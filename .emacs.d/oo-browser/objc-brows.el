;;!emacs
;;
;; FILE:         objc-brows.el
;; SUMMARY:      Objective-C source code browser.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     c, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:    12-Dec-89
;; LAST-MOD:     10-May-01 at 12:54:09 by Bob Weiner
;;
;; Copyright (C) 1989-1995, 1997  BeOpen.com
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;    Use 'objc-browse' to invoke the Objective-C OO-Browser.  Prefix arg
;;    prompts for name of Environment file.
;;
;; DESCRIP-END.

(provide 'objc-brows)

(require 'br)
(require 'br-objc-ft)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;; Cases
;; 
;; env-file = nil
;;   Use default environment, objc-env-file
;;   if objc not loaded, load it
;;   if objc-env-file != br-env-file
;;      switch to objc
;; 
;; env-file = t
;;   Prompt for env
;;   if env != objc
;;      load it
;;   else if env != br-env
;;      switch to env
;; 
;; env-file = filename
;;   if env != objc-env
;;      
;; 
;; objc-env-file = br-env-file

;;;###autoload
(defun objc-browse (&optional env-file no-ui)
  "Invoke the Objective-C OO-Browser.
This allows browsing through Objective-C library and system class
hierarchies.  With an optional non-nil prefix argument ENV-FILE, prompt for
Environment file to use.  Alternatively, a string value of ENV-FILE is used
as the Environment file name.  See also the file \"br-help\"."
  (interactive "P")
  (let ((same-lang (equal br-lang-prefix objc-lang-prefix))
	(load-succeeded t)
	same-env)
    (if same-lang
	nil
      ;; Save other language Environment in memory
      (if br-lang-prefix (br-env-copy nil))
      (setq br-lang-prefix objc-lang-prefix
	    *br-save-wconfig* nil))
    ;; `same-env' non-nil means the new Env is the previous Env or the most
    ;; recent previous Env of the same language as the new Env
    (setq same-env (or (equal objc-env-file env-file)
		       (and (null env-file)
			    (or objc-lib-search-dirs objc-sys-search-dirs))))
    (cond
     (same-env
      ;; If we just switched languages, restore the cached data for the new
      ;; Environment.
      (if same-lang nil (br-env-copy t))
      ;; Environment may appear to be the same but its loading may have
      ;; been interrupted, so ensure all variables are initialized properly.
      (objc-browse-setup env-file)
      (if (or (null br-paths-htable) (equal br-paths-htable br-empty-htable))
	  (setq load-succeeded
		(br-env-try-load (or env-file br-env-file) br-env-file))))
     ;;
     ;; Create default Environment file specification if needed and none
     ;; exists.
     ;;
     (t (or env-file (file-exists-p objc-env-file)
	    (br-env-create objc-env-file objc-lang-prefix))
	(or env-file (setq env-file objc-env-file))
	;;
	;; Start browsing a new Environment.
	;;
	(objc-browse-setup env-file)
	(setq load-succeeded (br-env-init env-file same-lang nil))
	(if load-succeeded
	    (setq *br-save-wconfig* nil
		  objc-env-file br-env-file
		  objc-env-name br-env-name
		  objc-sys-search-dirs br-sys-search-dirs
		  objc-lib-search-dirs br-lib-search-dirs))))
    (cond (load-succeeded
	   (if no-ui
	       nil
	     (br-browse)
	     (or (and same-lang same-env) (br-refresh))))
	  (no-ui nil)
	  (t (message "(objc-browse): You must build the Environment to browse it.")))))

(defun objc-class-list-filter (class-list top-only-flag)
  "Return CLASS-LIST sans any protocol or class category entries.
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
	     ;; class category
	     ((string-match "\(" class) nil)
	     ;; protocol / abstract class
	     ((string-match "\\`<" class)
	      (if br-protocols-with-classes-flag
		  (if (br-get-parents class)
		      nil
		    class)
		(objc-class-list-filter
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
			    (if (string-match "[\(\<]" class)
				nil
			      class)))
			 class-list)))))

(defun objc-mode-setup ()
  "Load best available Objective-C major mode and set 'br-lang-mode' to the function that invokes it."
  (defalias 'br-lang-mode
    (cond ((or (fboundp 'objc-mode) (featurep 'objc-mode)) 'objc-mode)
	  ((load "objc-mode" t 'nomessage) 'objc-mode)
	  ((featurep 'c-mode) 'c-mode)
	  ((load "cc-mode" 'missing-ok 'nomessage)
	   (if (fboundp 'objc-mode) 'objc-mode 'c-mode))
	  ((load "c-mode" nil 'nomessage)
	   (provide 'c-mode))))
  (condition-case ()
      (progn (require 'cc-mode)
	     (c-initialize-cc-mode))
    (error nil)))

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(defun objc-browse-setup (env-file)
  "Setup language-dependent functions for OO-Browser."
  (br-setup-functions)
  (objc-mode-setup)
  (br-setup-constants env-file)
  ;; Setup to add default classes ([category] and [protocol]) to system class
  ;; table after building it.  This must come after br-setup-constants call
  ;; since it clears these hooks.
  (if (fboundp 'add-hook)
      (add-hook 'br-after-build-sys-hook 'objc-add-default-classes)
    (setq br-after-build-sys-hook '(objc-add-default-classes))))
