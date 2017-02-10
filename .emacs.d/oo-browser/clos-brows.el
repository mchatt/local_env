;;!emacs
;;
;; FILE:         clos-brows.el
;; SUMMARY:      Common Lisp/CLOS source code browser.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     lisp, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:    29-Jul-90
;; LAST-MOD:     10-May-01 at 12:49:43 by Bob Weiner
;;
;; Copyright (C) 1990-1995, 1997  BeOpen.com
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;    Use 'clos-browse' to invoke the CLOS OO-Browser.  Prefix arg prompts for
;;    name of Environment file.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(mapcar 'require '(br-start br br-clos-ft))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun clos-browse (&optional env-file no-ui)
  "Invoke the CLOS OO-Browser.
This allows browsing through CLOS library and system class hierarchies.  With
an optional non-nil prefix argument ENV-FILE, prompt for Environment file
to use.  Alternatively, a string value of ENV-FILE is used as the
Environment file name.  See also the file \"br-help\"."
  (interactive "P")
  (let ((same-lang (equal br-lang-prefix clos-lang-prefix))
	(load-succeeded t)
	same-env)
    (if same-lang
	nil
      ;; Save other language Environment in memory
      (if br-lang-prefix (br-env-copy nil))
      (setq br-lang-prefix clos-lang-prefix
	    *br-save-wconfig* nil))
    ;; `same-env' non-nil means the new Env is the previous Env or the most
    ;; recent previous Env of the same language as the new Env
    (setq same-env (or (equal clos-env-file env-file)
		       (and (null env-file)
			    (or clos-lib-search-dirs clos-sys-search-dirs))))
    (cond
     (same-env
      ;; If we just switched languages, restore the cached data for the new
      ;; Environment.
      (if same-lang nil (br-env-copy t))
      ;; Environment may appear to be the same but its loading may have
      ;; been interrupted, so ensure all variables are initialized properly.
      (clos-browse-setup env-file)
      (if (or (null br-paths-htable) (equal br-paths-htable br-empty-htable))
	  (setq load-succeeded
		(br-env-try-load (or env-file br-env-file) br-env-file))))
     ;;
     ;; Create default Environment file specification if needed and none
     ;; exists.
     ;;
     (t (or env-file (file-exists-p clos-env-file)
	    (br-env-create clos-env-file clos-lang-prefix))
	(or env-file (setq env-file clos-env-file))
	;;
	;; Start browsing a new Environment.
	;;
	(clos-browse-setup env-file)
	(setq load-succeeded (br-env-init env-file same-lang nil))
	(if load-succeeded
	    (setq *br-save-wconfig* nil
		  clos-env-file br-env-file
		  clos-env-name br-env-name
		  clos-sys-search-dirs br-sys-search-dirs
		  clos-lib-search-dirs br-lib-search-dirs))))
    (cond (load-succeeded
	   (if no-ui
	       nil
	     (br-browse)
	     (or (and same-lang same-env) (br-refresh))))
	  (no-ui nil)
	  (t (message "(clos-browse): You must build the Environment to browse it.")))))

;; Don't filter Environment classes when listed.
(defalias 'clos-class-list-filter 'br-class-list-identity)

(defun clos-class-definition-regexp (class)
  "Return regexp to uniquely match the definition of CLASS name."
  (concat clos-class-name-before (regexp-quote class)
	  clos-class-name-after))

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(defun clos-browse-setup (env-file)
  "Setup language-dependent functions for OO-Browser."
  (br-setup-functions)
  (defalias 'br-lang-mode
	(cond ((featurep 'clos-mode) 'clos-mode)
	      ((load "clos-mode" 'missing-ok 'nomessage)
	       (provide 'clos-mode))
	      (t 'clos-browse-mode)))
  (br-setup-constants env-file)
  ;; Setup to add default classes to system class table after building it.
  ;; This must come after br-setup-constants call since it clears these
  ;; hooks.
  (if (fboundp 'add-hook)
      (add-hook 'br-after-build-sys-hook 'clos-add-default-classes)
    (setq br-after-build-sys-hook '(clos-add-default-classes))))

(defun clos-browse-mode ()
  "Select major mode for browsing the current buffer's file."
  (interactive)
  (if (and (stringp buffer-file-name)
	   (not (memq major-mode '(lisp-mode emacs-lisp-mode))))
      (cond ((string-match "\\.el$" buffer-file-name)
	     (emacs-lisp-mode))
	    (t (lisp-mode)))))

(provide 'clos-brows)
