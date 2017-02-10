;;!emacs
;;
;; FILE:         br-java.el
;; SUMMARY:      Support routines for Java inheritance browsing.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     c, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:    01-Aug-95
;; LAST-MOD:     10-May-01 at 05:39:36 by Bob Weiner
;;
;; Copyright (C) 1995, 1996, 1997  BeOpen.com
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
(require 'hypb)
(require 'br-c-ft)
(require 'hasht)

;;; ************************************************************************
;;; User visible variables
;;; ************************************************************************

(defvar   java-class-keyword
  "\\(class\\|interface\\)[ \t\n\r]+"
  "*Keyword regexp preceding a java class declaration or definition.")

(defvar   java-lib-search-dirs nil
  "List of directories below which java Library source files are found.
Subdirectories of Library source are also searched.  A Library is a stable
group of classes.")

(defvar   java-sys-search-dirs nil
  "List of directories below which java System source files are found.
Subdirectories of System source are also searched.  A System class is one
that is not yet reusable and is likely to change before release.")

(defvar   java-package-name nil
  "Name of current packge if any.  Nil otherwise.")

(defconst java-narrow-view-to-class nil
 "*Non-nil means narrow buffer to just the matching class definition when displayed.")

;;; ************************************************************************
;;; Internal variables
;;; ************************************************************************

(defconst java-class-modifier-keyword
  "\\(public[ \t\n\r]+\\|final[ \t\n\r]+\\|abstract[ \t\n\r]+\\)*")

(defconst java-class-name-before
  (concat "^[ \t]*" java-class-modifier-keyword java-class-keyword)
  "Regexp preceding the class name in a class definition.")

(defconst java-class-name-after
  "[ \t\n\r]*\\(\{\\|extends[^\{\(\;]+\\|implements[^\{\(\;]+\\)"
  "Regexp following the class name in a class definition.
Last character matched is a curly brace if the class does not implement any
protocols or inherit explicitly from any classes.")

(defconst java-identifier-chars "_$.a-zA-Z0-9"
  "String of chars and char ranges that may be used within a Java identifier.")

(defconst java-return-type-chars java-identifier-chars
  "String of chars and char ranges that may be used within a Java return type identifier.")

(defconst java-identifier (concat "\\([_$a-zA-Z][" java-identifier-chars "]*\\)")
  "Regular expression matching a Java identifier.")

(defconst java-class-def-regexp
  (concat java-class-name-before java-identifier java-class-name-after)
  "Regular expression used to match to class definitions in source text.
The `java-class-type-grpn' grouping matches to one of the literals `class' or
`interface', indicating the type of declaration found.  The class name
identifier is grouping `java-class-def-name-grpn'.  The
first part of the `java-class-def-derived-grpn' grouping matches to the first
`extends' or `implements' keyword, unless the class does not explicitly
inherit from any class or interface, in which case this grouping matches to
`\{'.")

(defconst java-class-type-grpn 2)
(defconst java-class-def-name-grpn 3)
(defconst java-class-def-derived-grpn 4)

(defconst java-lang-prefix "java-"
 "Prefix string that starts \"br-java.el\" symbol names.")

;; Sample expressions:
;;   class DataOutputStream extends FilterOutputStream implements DataOutput {
;;   class VectorEnumerator implements Enumeration {
;;   class Hashtable extends Dictionary implements Cloneable {
;;   class RandomAccessFile implements DataOutput, DataInput {
(defconst java-class-parent-regexp
  (concat "\\(implements\\|extends\\|,\\)[ \t\n\r]*" java-identifier "[ \t\n\r]*")
  "Matches to a single class or interface name inherited by a class.
Keyword or comma preceding the name is group `java-parent-prefix-grpn'.
The name itself is group `java-parent-name-grpn'.")
;; part 2 of original
;;	  "\\(\\(public\\|private\\|protected\\|final\||abstract\\|implements\\|extends\\)[,]?[ \t\n\r]+\\)?\\)?"

(defconst java-parent-prefix-grpn 1)
(defconst java-parent-name-grpn 2)

(defconst java-interface-parent-regexp
  (concat "\\(extends\\|,\\)[ \t\n\r]*" java-identifier "[ \t\n\r]*")
  "Matches to a single interface name inherited by an interface.
The `extends' keyword or the comma preceding the name is group
`java-parent-prefix-grpn'.  The name itself is group
`java-parent-name-grpn'.")

(defconst java-package-name-regexp
  (concat "[ \t\n\r]*" java-identifier "[ \t\n\r]*\;")
  "Regexp matching a package statement.  Package name is java-package-name-grpn.")

(defconst java-package-name-grpn 1)

(defconst java-package-word-regexp
  "\\([a-zA-z_0-9]*\\)\\.?"
   "Return a single component of a package name.")

(defconst java-static-init-regexp
  "[ \t\n\r]*static[ \t\n\r]+\{"
  "Regexp matching start of static initializer block.")

(defvar java-package-htable
  (hash-make 7)
  "Hash table of split package names.")

(defconst java-src-file-regexp "[^.]\\.\\(java\\)$"
  "Regular expression matching a unique part of java source or header file name and no others.")

(defvar java-children-htable nil
  "Htable whose elements are of the form: (LIST-OF-CHILD-CLASSES . CLASS-NAME).
Used to traverse java inheritance graph.  `br-build-children-htable' builds
this list.")
(defvar java-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Used to traverse java inheritance graph.  `br-build-parents-htable' builds
this list.")
(defvar java-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
`br-build-paths-htable' builds this list.")


(defvar java-lib-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from stable software libraries are used to build the list.")
(defvar java-lib-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from stable software libraries are used to build the list.")

(defvar java-sys-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from systems that are likely to change are used to build the
list.")
(defvar java-sys-paths-htable nil
  "Alist whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from systems that are likely to change are used to build the
list.")

(defvar java-lib-prev-search-dirs nil
  "Used to check if `java-lib-classes-htable' must be regenerated.")
(defvar java-sys-prev-search-dirs nil
  "Used to check if `java-sys-classes-htable' must be regenerated.")

(defvar java-env-spec nil
  "Non-nil value means Environment specification has been given but not yet built.
Nil means current Environment has been built, though it may still require
updating.")

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(defun java-get-classes-from-source (filename &optional skip-tags
				     skip-tags-cleanup)
  "Scans FILENAME and returns cons of class list with parents-class alist.
Handles multiple inheritance.  Assumes file existence and readability have
already been checked.
   With optional SKIP-TAGS non-nil, does not compute and store lookup tags
for member definitions.  If SKIP-TAGS is nil, normally a cleanup
function is called after scanning the members.  SKIP-TAGS-CLEANUP
non-nil suppresses this action."
  (let ((no-kill (get-file-buffer filename))
	class-name-end classes class has-parents interface-p
	open-brace-point end parents-start parents parent-cons parent-list
	signatures superclass-flag)
    (if no-kill
	(set-buffer no-kill)
      (funcall br-view-file-function filename))
    ;; Static initializers confuse the parser and don't define anything
    ;; that we need, so remove them.
    (java-strip-static-code)
    ;; Is more than one package statement allowed?
    (setq java-package-name (java-get-package-name))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward java-class-def-regexp nil t)
	  (setq has-parents
		(not (eq ?\{ (char-after (match-beginning
					  java-class-def-derived-grpn))))
		parents-start (match-beginning java-class-def-derived-grpn)
		interface-p (string-equal
			     (br-buffer-substring
			      (match-beginning java-class-type-grpn)
			      (match-end java-class-type-grpn))
			     "interface")
		end (match-end 0)
		class-name-end (match-end java-class-def-name-grpn)
		;;
		;; Now since we've saved all the match expressions we need
		;; from our last regexp match, we can call functions which
		;; change the match data below here.
		class (java-normalize-class-match)
		parent-list nil
		superclass-flag nil)
	  (if interface-p (setq class (concat "<" class ">")))
	  (goto-char parents-start)
	  (if has-parents
	      ;; Return parents as a list.
	      (if interface-p
		  (setq parent-list (java-scan-interface-parents end))
		(setq parent-list (java-scan-class-parents end)
		      superclass-flag (car parent-list)
		      parent-list (cdr parent-list))))
	  (if (not (or superclass-flag interface-p (equal class "Object")))
	      ;; All classes have Object as an ancestor, so if
	      ;; no superclass was found, add Object to the parent-list.
	      (setq parent-list (cons "Object" parent-list)))
	  ;;
	  ;; Ensure class name is not within a comment.
	  (if (c-within-comment-p)
	      (progn (search-forward "*/" nil t)
		     (setq class nil parent-cons nil))
	    (setq parent-cons (cons parent-list class)
		  classes (cons class classes)
		  parents (cons parent-cons parents))
	    (or skip-tags
		(progn
		  (if interface-p
		      ;; Add this interface def to the default
		      ;; [interface] class.
		      (setq signatures
			   (cons (java-feature-normalize
				  class java-default-interface-class
				  class)
				 signatures)))
		  ;; Scan members defined within class or interface
		  (goto-char class-name-end)
		  (if (search-forward "\{" nil t)
		      (progn (setq open-brace-point (point))
			     (backward-char)
			     ;; Move to class close brace but ignore
			     ;; any error if braces are unbalanced.
			     ;; Let the compiler tell the user about
			     ;; this.
			     (if (condition-case ()
				     (progn (forward-sexp) t)
				   (error nil))
				 (setq signatures
				       (append
					signatures
					(java-scan-features
					 class open-brace-point
					 (point)))))))))))))
    (if skip-tags
	nil
      (java-output-feature-tags filename signatures)
      (or skip-tags-cleanup (br-feature-build-htables)))
    (or no-kill
	(progn (set-buffer-modified-p nil)
	       (kill-buffer (current-buffer))))
    (cons classes (delq nil parents))))

(defun java-get-package-name()
  "Return the package name of the current file."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward java-package-name-regexp nil t)
	(br-buffer-substring (match-beginning java-package-name-grpn)
			     (match-end java-package-name-grpn))
      "")))

(defun java-split-identifier (name)
  "Return list of component words (in reverse order) of the given NAME."
  (or (hash-lookup name java-package-htable)
      (let ((s name)
	    start words tmp)
	(while (and (not (null s)) (> (length s) 0))
	  (setq start (string-match java-package-word-regexp s))
	  (if start
	      (progn
		(setq tmp (substring s (match-beginning 1) (match-end 1)))
		(setq s (substring s (match-end 0)))
		(setq words (cons tmp words)))))
	(hash-add words java-package-name java-package-htable))))

(defun java-normalize-class-name (name)
  "Normalize class NAME to include the package name that defines it."
  ;; Currently incomplete.  The defined class has a package name, but
  ;; the parents do not.  How do we match the parents to the correct
  ;; class if there are multiple matches?
  (or (car (java-split-identifier name))
      (if (null java-package-name)
	  (car (java-split-identifier name))
	;; Note: maybe allow user to pick how many words to prepend.
	(let ((prefix (car (java-split-identifier java-package-name))))
	  (if (and prefix (> (length prefix) 0))
	      (concat prefix "." (car (java-split-identifier name)))
	    (car (java-split-identifier name)))))))

(defun java-class-definition-regexp (class &optional regexp-flag)
  "Return regexp to uniquely match the definition of CLASS name.
Optional REGEXP-FLAG non-nil means CLASS has already been quoted for use in a
regular expression."
  (if (string-match "\\`<\\(.+\\)>\\'" class)
      ;; Strip interface delimiters which don't appear within class or
      ;; interface declarations.
      (setq class (substring class (match-beginning 1) (match-end 1))))
  (concat "^[ \t]*" java-class-modifier-keyword 
	  java-class-keyword (if regexp-flag class (regexp-quote class))
	  java-class-name-after))

(defun java-normalize-class-match ()
  "After a regexp match to a class definition, return the matching class name."
    (java-normalize-class-name
     (br-buffer-substring (match-beginning java-class-def-name-grpn)
			  (match-end java-class-def-name-grpn))))

(defun java-scan-class-parents (end)
  "Return (superclass-flag parent-list) from a java class declaration preceding END point.
The superclass-flag is t if the class has an actual concrete parent.  The
parent-list contains that parent and any interfaces to which the class
directly conforms.  Although Java permits only single inheritance via the
`extends' clause, the `implements' clause supports inheritance of multiple
interfaces and thus allows classes to have a limited form of multiple
parents.  Point must be before the `implements' or `extends' keyword that
precedes the first parent name."
  (let ((superclass-flag) (superclass-here) parent-list parent)
    (while (re-search-forward java-class-parent-regexp end t)
      (setq superclass-here
	    (string-equal (br-buffer-substring
			   (match-beginning java-parent-prefix-grpn)
			   (match-end java-parent-prefix-grpn))
			  "extends")
	    superclass-flag (or superclass-flag superclass-here)
	    parent (java-normalize-class-name
		    (br-buffer-substring
		     (match-beginning java-parent-name-grpn)
		     (match-end java-parent-name-grpn))))
      (if superclass-here
	  nil
	;; Mark as an interface
	(setq parent (concat "<" parent ">")))
      (setq parent-list (cons parent parent-list)))
    (cons superclass-flag (nreverse parent-list))))

(defun java-scan-interface-parents (end)
  "Return a list of the parent interfaces from a java interface declaration which precede END point.
Point must be before the `extends' keyword that precedes the first parent name."
  (let (parent-list parent)
    (while (re-search-forward java-interface-parent-regexp end t)
      (setq parent (concat "<"
			   (java-normalize-class-name
			    (br-buffer-substring
			     (match-beginning java-parent-name-grpn)
			     (match-end java-parent-name-grpn)))
			   ">")
	    parent-list (cons parent parent-list)))
    (nreverse parent-list)))

(defun java-get-parents-from-source (filename class)
  "Scan source in FILENAME and return list of parents of CLASS.
Assume file existence has already been checked."
    (cond ((null class) nil)
	  ((equal filename br-null-path)
	   ;; This means there is no source for this class, so 
	   ;; if this is not the Object class or an interface, since all
	   ;; other classes without parents have Object as an implicit
	   ;; parent, return Object as the sole parent.
	   (if (not (or (if (stringp class) (eq (aref class 0) ?\<))
			(equal class "Object")))
	       '("Object")))
	  (t (car (car (br-rassoc
			class
			(cdr (java-get-classes-from-source filename t))))))))

(defun java-select-path (paths-htable-elt &optional feature-p)
  "Select proper pathname from PATHS-HTABLE-ELT based upon value of optional FEATURE-P.
Selection is between path of class definition and path for features associated
with the class."
  (let ((elt (cdr paths-htable-elt)))
    (if (consp elt) 
	(if feature-p (cdr elt) (car elt))
      ;; Both paths are the same.
      elt)))

(defun java-set-case (type)
  "Return string TYPE identifier for use as a class name."
  type)

(defun java-set-case-type (class-name)
  "Return string CLASS-NAME for use as a type identifier."
  class-name)

(defun java-to-class-end ()
  "Assuming point is at start of class, move to start of line after end of class."
  (interactive)
  (condition-case ()
      (forward-list)
    (error (progn (or (re-search-forward "^}" nil t)
		      (goto-char (point-max))))))
  (forward-line 1))

(defalias 'java-to-comments-begin 'br-c-to-comments-begin)

;; Static initializers confuse the parser, and don't define anything
;; that we need
(defun java-strip-static-code ()
  "Strip the static initializers from this buffer."
  (let (buffer-read-only start open-brace)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward java-static-init-regexp (point-max) t)
	(setq start (match-beginning 0)
	      open-brace (1- (match-end 0)))
	(goto-char open-brace)
	(let ((before (point)))
	  (if (eq (following-char) ?\{)
	      (condition-case ()
		  (forward-sexp)
		(error nil)))
	  (delete-region before (point))
	  (delete-region start open-brace))))))

(provide 'br-java)
