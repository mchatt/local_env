;;!emacs
;;
;; FILE:         eif-browse.el
;; SUMMARY:      Eiffel source code browser.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:    12-Dec-89
;; LAST-MOD:     10-May-01 at 12:49:59 by Bob Weiner
;;
;; Copyright (C) 1989-1995, 1997  BeOpen.com
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;    Use `eif-browse' to invoke the Eiffel browser.  Prefix arg prompts for
;;    name of Environment file.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(mapcar 'require '(br-start br br-eif-ft))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun eif-browse (&optional env-file no-ui)
  "Invoke the Eiffel OO-Browser.
This allows browsing through Eiffel library and system class hierarchies.
With an optional prefix arg ENV-FILE equal to t, prompt for Environment file
to use.  Alternatively, a string value of ENV-FILE is used as the Environment
file name.  See also the file \"br-help\"."
  (interactive "P")
  (let ((same-lang (equal br-lang-prefix eif-lang-prefix))
	(load-succeeded t)
	same-env)
    (if same-lang
	nil
      ;; Save other language Environment in memory
      (if br-lang-prefix (br-env-copy nil))
      (setq br-lang-prefix eif-lang-prefix
	    *br-save-wconfig* nil))
    ;; `same-env' non-nil means the new Env is the previous Env or the most
    ;; recent previous Env of the same language as the new Env
    (setq same-env (or (equal eif-env-file env-file)
		       (and (null env-file)
			    (or eif-lib-search-dirs eif-sys-search-dirs))))
    (cond
     (same-env
      ;; If we just switched languages, restore the cached data for the new
      ;; Environment.
      (if same-lang nil (br-env-copy t))
      ;; Environment may appear to be the same but its loading may have
      ;; been interrupted, so ensure all variables are initialized properly.
      (eif-browse-setup env-file)
      (if (or (null br-paths-htable) (equal br-paths-htable br-empty-htable))
	  (setq load-succeeded
		(br-env-try-load (or env-file br-env-file) br-env-file))))
     ;;
     ;; Create default Environment file specification if needed and none
     ;; exists.
     ;;
     (t (or env-file (file-exists-p eif-env-file)
	    (br-env-create eif-env-file eif-lang-prefix))
	(or env-file (setq env-file eif-env-file))
	;;
	;; Start browsing a new Environment.
	;;
	(eif-browse-setup env-file)
	(setq load-succeeded (br-env-init env-file same-lang nil))
	(if load-succeeded
	    (setq *br-save-wconfig* nil
		  eif-env-file br-env-file
		  eif-env-name br-env-name
		  eif-sys-search-dirs br-sys-search-dirs
		  eif-lib-search-dirs br-lib-search-dirs))))
    (cond (load-succeeded
	   (if no-ui
	       nil
	     (br-browse)
	     (or (and same-lang same-env) (br-refresh))))
	  (no-ui nil)
	  (t (message "(eif-browse): You must build the Environment to browse it.")))))

;; Don't filter Environment classes when listed.
(defalias 'eif-class-list-filter 'br-class-list-identity)

(defun eif-class-definition-regexp (class)
  "Return regexp to uniquely match the definition of CLASS name."
  (concat eif-class-name-before (regexp-quote class)
	  eif-class-name-after))

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(defun eif-browse-setup (env-file)
  "Setup language-dependent functions for OO-Browser."
  (br-setup-functions)
  (defalias 'br-lang-mode
	(cond ((or (featurep 'eiffel3) (featurep 'eiffel-mode))
	       'eiffel-mode)
	      ((load "eiffel3" 'missing-ok 'nomessage)
	       (provide 'eiffel-mode))
	      ((load "eiffel" 'missing-ok 'nomessage)
	       (provide 'eiffel-mode))
	      (t 'fundamental-mode)))
  (br-setup-constants env-file)
  ;; Setup to add default classes to system class table after building it.
  ;; This must come after br-setup-constants call since it clears these
  ;; hooks.
  (if (fboundp 'add-hook)
      (add-hook 'br-after-build-sys-hook 'eif-add-default-classes)
    (setq br-after-build-sys-hook '(eif-add-default-classes))))

(provide 'eif-browse)
