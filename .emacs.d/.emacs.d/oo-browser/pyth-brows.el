;;!emacs
;;
;; FILE:         pyth-brows.el
;; SUMMARY:      Python source code browser.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     oop, tools, python
;;
;; AUTHOR:       Harri Pasanen / Bob Weiner
;;               based on Smalltalk and C++ OO-Browsers 
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:    5-Apr-96
;; LAST-MOD:     10-May-01 at 12:47:52 by Bob Weiner
;;
;; Copyright (C) 1996, 1997, 1998  BeOpen.com
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;    Use 'python-browse' to invoke the Python OO-Browser.  Prefix arg
;;    prompts for name of Environment file.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(mapcar 'require '(br-start br br-python-ft))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun python-browse (&optional env-file no-ui)
  "Invoke the Python OO-Browser.
This allows browsing through Python library and system class hierarchies.
With an optional non-nil prefix argument ENV-FILE, prompt for Environment
file to use.  Alternatively, a string value of ENV-FILE is used as the
Environment file name.  See also the file \"br-help\"."
  (interactive "P")
  (let ((same-lang (equal br-lang-prefix python-lang-prefix))
	(load-succeeded t)
	same-env)
    (if same-lang
	nil
      ;; Save other language Environment in memory
      (if br-lang-prefix (br-env-copy nil))
      (setq br-lang-prefix python-lang-prefix
	    *br-save-wconfig* nil))
    ;; `same-env' non-nil means the new Env is the previous Env or the most
    ;; recent previous Env of the same language as the new Env
    (setq same-env (or (equal python-env-file env-file)
		       (and (null env-file)
			    (or python-lib-search-dirs python-sys-search-dirs))))
    (cond
     (same-env
      ;; If we just switched languages, restore the cached data for the new
      ;; Environment.
      (if same-lang nil (br-env-copy t))
      ;; Environment may appear to be the same but its loading may have
      ;; been interrupted, so ensure all variables are initialized properly.
      (python-browse-setup env-file)
      (if (or (null br-paths-htable) (equal br-paths-htable br-empty-htable))
	  (setq load-succeeded
		(br-env-try-load (or env-file br-env-file) br-env-file))))
     ;;
     ;; Create default Environment file specification if needed and none
     ;; exists.
     ;;
     (t (or env-file (file-exists-p python-env-file)
	    (br-env-create python-env-file python-lang-prefix))
	(or env-file (setq env-file python-env-file))
	;;
	;; Start browsing a new Environment.
	;;
	(python-browse-setup env-file)
	(setq load-succeeded (br-env-init env-file same-lang nil))
	(if load-succeeded
	    (setq *br-save-wconfig* nil
		  python-env-file br-env-file
		  python-env-name br-env-name
		  python-sys-search-dirs br-sys-search-dirs
		  python-lib-search-dirs br-lib-search-dirs))))
    (cond (load-succeeded
	   (if no-ui
	       nil
	     (br-browse)
	     (or (and same-lang same-env) (br-refresh))))
	  (no-ui nil)
	  (t (message "(python-browse): You must build the Environment to browse it.")))))

;; Don't filter Environment classes when listed.
(defalias 'python-class-list-filter 'br-class-list-identity)

(defun python-lang-mode ()
  "Invoke normal-mode for programming language buffers and python-mode for any others."
  (normal-mode)
  (if (not (memq major-mode '(python-mode c-mode c++-mode java-mode)))
      (python-mode)))

(defun python-mode-setup ()
  "Load best available Python major mode and set 'br-lang-mode' to the function that invokes it."
  (defalias 'br-lang-mode 'python-lang-mode))

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(defun python-browse-setup (env-file)
  "Setup language-dependent functions for OO-Browser."
  (br-setup-functions)
  (python-mode-setup)
  (br-setup-constants env-file)
  ;; Setup to add default classes to system class table after building it.
  ;; This must come after br-setup-constants call since it clears these
  ;; hooks.
  (if (fboundp 'add-hook)
      (add-hook 'br-after-build-sys-hook 'python-add-default-classes)
    (setq br-after-build-sys-hook '(python-add-default-classes))))

(provide 'pyth-brows)
