;;!emacs
;;
;; FILE:         br-start.el
;; SUMMARY:      Select language and invoke OO-Browser.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:     5-Sep-92 at 23:31:03
;; LAST-MOD:      3-Aug-99 at 22:41:31 by Bob Weiner
;;
;; Copyright (C) 1992, 1993, 1994, 1997, 1998 BeOpen.com
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; OO-Browser directory setting
;;; ************************************************************************

;; Try to add hyperbole/ or OO-Browser-specific part, hypb/, to the load-path.
(if (eq t (catch 'found
	    (mapcar
	     (function (lambda (path)
			 (if (string-match "\\(/hyperbole\\|/hypb\\)/?\\'" path)
			     (throw 'found t))))
	     load-path)))
    nil
  (cond ((file-readable-p "hypb/")
	 (setq load-path (cons (expand-file-name "hypb/") load-path)))
	((file-readable-p "../hyperbole/")
	 (setq load-path (cons (expand-file-name "../hyperbole/") load-path)))))

;; May define `locate-file' which is used below.
;; A Hyperbole directory, such as oo-browser/hypb, must either already be in
;; load-path or an explicit load of "hversion" must have been
;; done already or else the following line will fail to load hversion.
;; This is all documented in the OO-Browser installation instructions.
(require 'hversion)

;; Reinitialize br-directory on reload if initialization failed for any reason.
(and (boundp 'br-directory) (null br-directory) (makunbound 'br-directory))

(defvar br-directory (or (locate-file "br-help-ms" load-path)
			 (and (file-readable-p "br-help-ms")
			      (expand-file-name "./")))
  "Directory where the OO-Browser executable code and help files are kept.
It must end with a directory separator character.")
(if (stringp br-directory)
    (setq br-directory (file-name-directory br-directory))
  (error
   "(br-start.el): OO-Browser failed to set br-directory.  Try setting it manually."))

;; Add OO-Browser Info directory to Info-directory-list after the Info
;; manual reader package is loaded.
(if (featurep 'info)
    (let ((info-dir (expand-file-name "man/" br-directory)))
      (if (file-exists-p info-dir)
	  (add-hook 'Info-directory-list info-dir t)))
  (add-hook 'Info-mode-hook
	    (function
	     (lambda ()
	       (if (boundp 'br-directory)
		   (let ((info-dir (expand-file-name "man/" br-directory)))
		     (if (file-exists-p info-dir)
			 (add-hook 'Info-directory-list info-dir t))))))))

(if (not (fboundp 'defalias)) (fset 'defalias 'fset))

(if (fboundp 'member)
    (defalias 'br-member 'member)
  (defun br-member (elt list)
    "Return non-nil if ELT is an element of LIST.  Comparison done with `equal'.
The value is actually the tail of LIST whose car is ELT."
    (while (and list (not (equal (car list) elt)))
      (setq list (cdr list)))
    list))

;;; ************************************************************************
;;; Hyperbole subset directory setting (for mouse handling)
;;; ************************************************************************

;; Reinitialize hyperb:dir on reload if initialization failed for any reason.
(and (boundp 'hyperb:dir) (null hyperb:dir) (makunbound 'hyperb:dir))

(defvar hyperb:dir (or (locate-file "hmouse-tag.elc" load-path)
		       (and (file-readable-p "../hyperbole/")
			    (expand-file-name "../hyperbole/"))
		       (and (file-readable-p "hypb/")
			    (expand-file-name "hypb/")))
  "Directory where the Hyperbole executable code is kept.
It must end with a directory separator character.")
(if (stringp hyperb:dir)
    (setq hyperb:dir (file-name-directory hyperb:dir))
  (error
   "(br-start.el): Failed to set hyperb:dir.  Try setting it manually."))

;;; ************************************************************************
;;; Other public variables
;;; ************************************************************************

(defconst br-ootags-executable
  (or (locate-file "ootags" (list br-directory exec-directory)
		   ":.exe")
      (locate-file "ootags" exec-path ":.exe"))
  "Full pathname to `ootags' executable or nil if not found.")

(defconst br-shell-executable
  (or (locate-file "bash" exec-path ":.exe")
      (locate-file "sh" exec-path ":.exe")
      (locate-file "csh" exec-path ":.exe"))
  "Full pathname to command shell executable or nil if not found.")

(defvar br-c-tags-flag
  (if hyperb:microcruft-os-p
      ;;
      ;; !! Temporarily disable c-tags building under microcruft systems
      ;; !! since is not working properly (it can hang the build).
      nil
      ;; Set to t on MS OSs only if a valid shell and ootags.exe is found.  This
      ;; is required by the "br-c-ft.el" code.
      ;; (and br-ootags-executable br-shell-executable t)
    t)
  "*Non-nil means add C constructs when building C-based language Environments.")

;; -f treats upper and lower case the same in sorting, also makes `a' sort
;; list before `[a]', so default classes appear at the end of the list,
;; typically.
;; -u leaves only unique elements in the sorted list
(defvar br-sort-options (if hyperb:microcruft-os-p nil "-fu")
  "*String of options to send to the operating system `sort' command.
Use nil for none.  The default sort command under MS OSes does not
support the options needed, so this must be set to nil in such cases.")

(defvar br-env-default-file "OOBR"
  "*Standard file name for OO-Browser Environment storage.")

(defvar br-env-file nil
  "Default file into which to save a class Environment.
Value is language-specific.")

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

;; Add br-directory and hyperb:dir to load-path so other OO-Browser libraries
;; can be found.
(or (br-member br-directory load-path)
    (setq load-path (cons br-directory load-path)))
(or (br-member hyperb:dir load-path)
    (setq load-path (cons hyperb:dir load-path)))

(load "br-vers")
(mapcar 'require '(br-init br-site))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun oo-browser (&optional same-env-flag)
  "Prompt for an Environment over which to run the OO-Browser.
Optional prefix argument SAME-ENV-FLAG means browse the current Environment,
if any, without prompting.  Otherwise, if called interactively, give the user
a choice whether to re-browse the last Environment or to browse a new one."
  (interactive
   (list (prog1
	     (if (and (not current-prefix-arg) br-env-file br-lang-prefix)
		 (y-or-n-p (format "(OO-Browser):  Browse `%s' again? "
				   (or (and (not (eq br-env-name t)) br-env-name)
				       (br-env-substitute-home br-env-file))))
	       current-prefix-arg)
	   (message ""))))
  (if (and same-env-flag br-env-file br-lang-prefix)
      (funcall (intern-soft (concat br-lang-prefix "browse")))
    (call-interactively 'br-env-browse)))

;;;###autoload
(defun br-two-button-mouse ()
  "Sets up the Action Key within OO-Browser listing buffers for a two button mouse.
The Action Key is placed on the left mouse button."
  (interactive)
  (setq hyperb:mouse-buttons 2)
  (br-setup-mouse-keys)
  (message
   "2-Button Mouse: Display OO-Browser listing items with the left mouse button."))

;;;###autoload
(defun br-three-button-mouse ()
  "Sets up the Action Key within OO-Browser listing buffers for a three button mouse.
The Action Key is placed on the middle mouse button."
  (interactive)
  (setq hyperb:mouse-buttons 3)
  (br-setup-mouse-keys)
  (message
   "3-Button Mouse: Display OO-Browser listing items with the middle mouse button."))

(provide 'br-start)
