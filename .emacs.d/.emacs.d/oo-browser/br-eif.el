;;!emacs
;;
;; FILE:         br-eif.el
;; SUMMARY:      Support routines for Eiffel inheritance browsing and error parsing.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:     7-Dec-89
;; LAST-MOD:     13-Jul-99 at 17:00:51 by Bob Weiner
;;
;; Copyright (C) 1989-1995, 1997  BeOpen.com
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'br-lib)

;;; ************************************************************************
;;; User visible variables
;;; ************************************************************************

(defvar eif-lib-search-dirs nil
  "List of directories below which Eiffel Library source files are found.
Subdirectories of Library source are also searched.  A Library is a stable
group of classes.")

(defvar eif-sys-search-dirs nil
  "List of directories below which Eiffel System source files are found.
Subdirectories of System source are also searched.  A System class is one
that is not yet reusable and is likely to change before release.")

(defconst eif-narrow-view-to-class nil
 "*Non-nil means narrow buffer to just the matching class definition when displayed.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun eif-get-classes-from-source (filename &optional skip-tags
				    skip-tags-cleanup)
  "Scans FILENAME and returns cons of class list with parents-class alist.
Handles multiple inheritance.  Assumes file existence and readability have
already been checked.
   With optional SKIP-TAGS non-nil, does not compute and store lookup tags
for element definitions.  If SKIP-TAGS is nil, normally a cleanup
function is called after scanning the elements.  SKIP-TAGS-CLEANUP
non-nil suppresses this action."
  ;; Multiple classes per file allowed
  (let (classes class end parents parent-cons signatures start)
    (funcall br-view-file-function filename)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward eif-class-def-regexp nil t)
	  (setq start (point)
		class
		(br-buffer-substring (match-beginning 2) (match-end 2))
		classes (cons class classes))
	  (eif-to-class-end)
	  (setq end (point)
		parents (eif-scan-class-parents end))
	  ;; All classes (aside from PLATFORM and GENERAL) have ANY as an
	  ;; ancestor, so if no superclass was found, add ANY to the list of
	  ;; `parents'.
	  (if (and (null parents)
		   (not (br-member class '("ANY" "PLATFORM" "GENERAL"))))
	      (setq parents (cons "ANY" parents)))
	  (setq parent-cons (cons parents class)
		parents (cons parent-cons parents))
	  ;;
	  (goto-char end)
	  (or skip-tags
	      ;; Scan class features
	      (setq signatures
		    (eif-scan-features-in-class class start end))))))
    (if skip-tags
	nil
      (eif-output-feature-tags filename signatures)
      (or skip-tags-cleanup (br-feature-build-htables)))
    (cons classes (delq nil parents))))

(defun eif-get-parents-from-source (filename class)
  "Scan source in FILENAME and return list of parents of CLASS.
Assume file existence has already been checked."
    (cond ((null class) nil)
	  ((equal filename br-null-path)
	   ;; This means there is no source for this class, so
	   ;; if this is not one of the ANY, PLATFORM or GENERAL classes,
	   ;; return ANY as the sole parent.
	   (if (not (br-member class '("ANY" "PLATFORM" "GENERAL")))
	       '("ANY")))
	  (t (car (car (br-rassoc
			class
			(cdr (eif-get-classes-from-source filename t nil))))))))

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(defun eif-scan-class-parents (end)
  "Return list of parents from an Eiffel class declaration preceding END point."
  (let ((parents) (par)
	(case-fold-search t) ;; Ignore case in searches
	found indent)
    (goto-char (point-min))
    (while (and (setq found (re-search-forward
			     (concat "\\<inherit[ \t\n\r]+"
				     eif-parent-regexp)
			     end t))
		(eif-in-comment-p)))
    (if (not found)
	nil
      ;; Save first parent
      (setq parents (list
		     (br-buffer-substring (match-beginning 2)
					  (match-end 2)))
	    indent (save-excursion
		     (goto-char (match-beginning 2))
		     (current-column)))
      ;; Save any succeeding parents
      (save-excursion
	(if (re-search-forward "^[a-zA-Z]" nil t)
	    (setq end (1- (point)))))
      (forward-line 1)
      (while (< (point) end)
	(back-to-indentation)
	(and (<= (current-column) indent)
	     (looking-at eif-identifier)
	     (setq par (br-buffer-substring (match-beginning 1)
					    (match-end 1)))
	     (if (or (br-member par parents)
		     (hash-key-p par eif-reserved-words-htable))
		 nil
	       (setq parents (cons par parents))))
	(forward-line 1)))
    (nreverse parents)))

(defun eif-select-path (paths-htable-elt &optional feature-p)
  "Select proper pathname from PATHS-HTABLE-ELT based upon value of optional FEATURE-P.
Selection is between path of class definition and path for features associated
with the class."
  (cdr paths-htable-elt))

;; Return string TYPE identifier for use as a class name.
(defalias 'eif-set-case 'identity)

(defun eif-set-case-type (class-name)
  "Return string CLASS-NAME for use as a type identifier."
  (upcase class-name))


(defun eif-to-class-end ()
  "Assuming point is at start of class, move to start of line after end of class."
  (interactive)
  (if (and (re-search-forward "^end[ \t\n\r\f-]" nil t)
	   (= (forward-line 1) 0))
      nil
    (goto-char (point-max))))

(defun eif-to-comments-begin ()
  "Skip back from current point past any preceding blank lines and comments."
  (let ((opoint))
    (while
	(progn (setq opoint (point))
	       ;; To previous line
	       (and (= 0 (forward-line -1))
		    ;; If begins with "--", then is a comment.
		    (cond ((looking-at "[ \t]*--"))
			  ((looking-at "[ \t]*$"))))))
    (goto-char opoint)
    ;; Skip past whitespace
    (skip-chars-forward " \t\n\r\f")
    (beginning-of-line)))

;;; ************************************************************************
;;; Internal variables
;;; ************************************************************************

(defconst eif-class-name-before "^[ \t]*\\(deferred[ \t\n\r]+\\|expanded[ \t\n\r]+\\)?class[ \t\n\r]+"
  "Regexp preceding the class name in a class definition.")

(defconst eif-class-name-after "[ \t\n\r]+"
  "Regexp following the class name in a class definition.")

(defconst eif-identifier-chars "A-Za-z0-9_"
  "String of chars and char ranges that may be used within an Eiffel identifier.")

(defconst eif-identifier (concat "\\([a-zA-Z][" eif-identifier-chars "]*\\)")
  "Regular expression matching an Eiffel identifier.")

(defconst eif-class-def-regexp
  (concat eif-class-name-before eif-identifier eif-class-name-after)
  "Regular expression used to match to class definitions in source text.
Class name identifier is grouped expression 2.")

(defconst eif-class-name-preceding
  "\\([\[\{>;:][ \t\n\r]*\\|[a-zA-z][ \t\n\r]+\\)"
  "Pattern preceding any valid non-comment use of an Eiffel class/type name.")

(defconst eif-class-name-pat
  (concat eif-class-name-preceding eif-identifier)
  "Class name is grouped expression 2.")

(defconst eif-lang-prefix "eif-"
  "Prefix string that starts \"br-eif.el\" symbol names.")


(defconst eif-parent-regexp (concat "[ \t\n\r]*\\(--.*[\n]\\)*[ \t\n\r]*"
				    eif-identifier)
  "Parent identifier is grouped expression 2.")

(defconst eif-src-file-regexp ".\\.e$"
  "Regular expression matching a unique part of Eiffel class filenames and no others.")

(defvar eif-children-htable nil
  "Htable whose elements are of the form: (LIST-OF-CHILD-CLASSES . CLASS-NAME).
Used to traverse Eiffel inheritance graph.  `br-build-children-htable' builds
this list.")
(defvar eif-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Used to traverse Eiffel inheritance graph.  `br-build-parents-htable' builds
this list.")
(defvar eif-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . DIRECTORY).
DIRECTORY gives the location of classes found in LIST-OF-CLASS-NAMES.
`br-build-paths-htable' builds this list.")

(defvar eif-lib-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from stable software libraries are used to build the list.")
(defvar eif-lib-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . DIRECTORY).
DIRECTORY gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from stable software libraries are used to build the list.")

(defvar eif-sys-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from systems that are likely to change are used to build the list.")
(defvar eif-sys-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . DIRECTORY).
DIRECTORY gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from systems that are likely to change are used to build the
list.")

(defvar eif-lib-prev-search-dirs nil
  "Used to check if `eif-lib-paths-htable' must be regenerated.")
(defvar eif-sys-prev-search-dirs nil
  "Used to check if `eif-sys-paths-htable' must be regenerated.")

(defvar eif-env-spec nil
  "Non-nil value means Environment specification has been given but not yet built.
Nil means current Environment has been built, though it may still require updating.")

(provide 'br-eif)
