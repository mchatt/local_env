;;!emacs
;;
;; FILE:         br-python.el
;; SUMMARY:      Support routines for Python inheritance browsing.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     oop, tools, python
;;
;; AUTHOR:       Harri Pasanen / Bob Weiner
;;               based on Smalltalk and C++ OO-Browsers 
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:    5-Apr-96
;; LAST-MOD:     10-May-01 at 17:39:56 by Bob Weiner
;;
;; Copyright (C) 1996, 1997, 1998  BeOpen.com
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;   See 'python-class-def-regexp' for regular expression that matches class
;;   definitions.
;;            
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(mapcar 'require '(br-lib hypb br-c-ft))

;;; ************************************************************************
;;; User visible variables
;;; ************************************************************************

(defvar python-lib-search-dirs nil
  "List of directories below which Python Library source files are found.
Subdirectories of Library source are also searched.  A Library is a stable
group of classes.")

(defvar python-sys-search-dirs nil
  "List of directories below which Python System source files are found.
Subdirectories of System source are also searched.  A System class is one
that is not yet reusable and is likely to change before release.")

(defconst python-narrow-view-to-class nil
 "*Non-nil means narrow buffer to just the matching class definition when displayed.")

;;; following is reserved for future, currently does not work
(defconst python-duplicate-classnames-across-modules nil
 "*Non-nil means that the module name is prepended to class names.")

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(defun python-get-classes-from-source (filename &optional skip-tags
						skip-tags-cleanup)
  "Scans FILENAME and returns cons of class list with parents-class alist.
Handles multiple inheritance.  Assumes file existence and readability have
already been checked."
  (let ((no-kill (get-file-buffer filename))
	(case-fold-search)
	(signatures)
	classes class has-parents parents parent-cons)
    (if no-kill
	(set-buffer no-kill)
      (funcall br-view-file-function filename))
    (save-restriction
      (save-excursion
	(widen)
	(goto-char (point-min))
	(or skip-tags
	    (progn (setq signatures (cons (python-module-tag filename)
					  (python-scan-features)))
		   (goto-char (point-min))))
	(while (re-search-forward python-class-def-regexp nil t)
	  (setq has-parents (eq ?\( (following-char))
		class (br-buffer-substring (match-beginning 1) (match-end 1))
		parent-cons 
		(cons
		 ;; Return parents as a list, or nil if no parents
		 (if has-parents (python-scan-parents))
		 class))
	  (if (python-within-string-p)
	      ;; ignore any class found and skip to the end of the string
	      (progn (re-search-forward python-multi-line-string-delimiter nil t)
		     (setq class nil parents nil))
	    (setq classes (cons class classes)
		  parents (cons parent-cons parents))
	    (or skip-tags
		;; Scan members defined within class
		(setq signatures
		      (append
		       signatures
		       (python-scan-features-in-class class (point) 
						      (python-locate-class-end))
		       ))))))
      (if skip-tags
	  nil
	(python-output-feature-tags filename signatures)
	(or skip-tags-cleanup (br-feature-build-htables)))
      (or no-kill (kill-buffer (current-buffer)))
      (cons classes (delq nil parents)))))


(defun python-scan-parents ()
  "Return list of parents names from a Python class definition.
Point must be after the '(' that begins the parent list and before the
first parent entry when this function is called."
  (let ((parent-list) (again t)
	parent)
    (while (and again (re-search-forward python-parent-class-name nil t))
      (setq again (eq ?, (following-char))
	    parent (br-buffer-substring (match-beginning 3)
					(match-end 3))
	    parent-list (cons parent parent-list)))
    (nreverse parent-list)))

(defun python-locate-class-end ()
  "Look up the end of class.  Point is assumed to be in the class definition.
Do this by looking up the first line that begins with python-identifier-chars."
  (save-excursion
    (let ((result-point (point-max)))
      (if (re-search-forward 
	   (concat "^[" python-identifier-chars "]") nil t)
	  (progn
	    (setq result-point (- (match-beginning 0) 1))
	    (if (python-within-string-p)
		(python-locate-class-end)
	      result-point))
	result-point))))

(defun python-get-parents-from-source (filename class-name)
  "Scan source in FILENAME and return list of parents of CLASS-NAME.
Assume file existence has already been checked."
    (or (null class-name)
	(car (car (br-rassoc
		   class-name
		   (cdr (python-get-classes-from-source filename)))))))

(defun python-select-path (paths-htable-elt &optional feature-p)
  "Select proper pathname from PATHS-HTABLE-ELT based upon value of optional FEATURE-P.
Selection is between path of class definition and path for features associated
with the class."
  (cdr paths-htable-elt))

(defun python-set-case (type)
  "Return string TYPE identifier for use as a class name."
  type)

(defun python-set-case-type (class-name)
  "Return string CLASS-NAME for use as a type identifier."
  class-name)

(defun python-to-comments-begin ()
  "Skip back from current point past any preceding Python comments at the beginning of lines."
  (if (eq major-mode 'python-mode)
      (let ((opoint))
	(while (progn (setq opoint (point))
		      ;; To previous line
		      (if (zerop (forward-line -1))
			  (cond ((looking-at "[ \t]*\\(#\\|$\\)"))
				(t nil)))))
	(goto-char opoint)
	;; Skip past whitespace
	(skip-chars-forward " \t\n\r\f")
	(beginning-of-line))
    ;; Assume this is a buffer of C/C++/Java code.
    (br-c-to-comments-begin)))

(defun python-class-definition-regexp (class &optional regexp-flag)
  "Return regexp to uniquely match the definition of CLASS name.
Matches past the signature end colon or the argument list open parenthesis.
Optional REGEXP-FLAG non-nil means CLASS has already been quoted for use in a
regular expression."
  (concat python-class-name-before
	  (if regexp-flag class (regexp-quote class))
	  "[ \t\n\r]*[:\(]"))

;;; Package and module support

(defun python-module-name (&optional module-file)
  "Return a module name derived from MODULE-FILE (a pathname) or from `buffer-file-name'."
  (file-name-sans-extension (file-name-nondirectory
			     (or module-file buffer-file-name))))

(defun python-module-tag (module-file)
  "Return a module tag string for MODULE-FILE."
  (format "[module]%s%% %s%s"
	  python-type-tag-separator
	  (python-module-name module-file)
	  python-type-tag-separator))

(defun python-add-package (dir)
  "Add Python package DIR to the feature tags file."
  (python-output-feature-tags
   dir (list (format "[package]%s%% %s%s"
		     python-type-tag-separator
		     (file-name-nondirectory (directory-file-name dir))
		     python-type-tag-separator))))

(defun python-search-directory (dir files)
  (mapcar
   (function
    (lambda (f)
      (if (string-equal (file-name-nondirectory f) "__init__.py")
	  ;; Mark this as a package directory and skip this file.
	  (python-add-package dir)
	(if (file-readable-p f)
	    (progn (message "Scanning %s in %s ..."
			    (file-name-nondirectory f)
			    (br-abbreviate-file-name
			     (or (file-name-directory f) default-directory)))
		   (setq paths-parents-cons
			 (br-get-classes-from-source f nil t)
			 classes (car paths-parents-cons)
			 parents (cdr paths-parents-cons)
			 br-paths-alist
			 (if classes
			     (cons (cons classes f) br-paths-alist)
			   br-paths-alist)
			 br-parents-alist (if parents
					      (append br-parents-alist
						      parents)
					    br-parents-alist)))
	  ;; else
	  (message "(OO-Browser):  Unreadable file: %s in %s"
		   (file-name-nondirectory f)
		   (br-abbreviate-file-name
		    (or (file-name-directory f) default-directory)))
	  (sit-for 1)))))
   ;; List of files potentially containing classes.
   (delq nil
	 (mapcar
	  (function
	   (lambda (f)
	     (and (string-match br-src-file-regexp f)
		  (not (file-directory-p f))
		  f)))
	  files))))

;;; ************************************************************************
;;; Internal variables
;;; ************************************************************************

(defconst python-identifier-chars "a-zA-Z0-9_"
  "String of chars and char ranges that may be used within a Python identifier.")

(defconst python-identifier (concat "\\([a-zA-Z_][" python-identifier-chars "]*\\)")
  "Regular expression matching a Python identifier.")

(defconst python-global-name
  (concat "[ \t]*" python-identifier "[ \t]*")
  "Regular expression matching a single identifier within a 'global' statement.
Grouped expression 1 is matches the identifier.")

(defconst python-parent-class-name
  (concat "[ \t]*" 
	  "\\(" python-identifier "\\.\\)*" ; possible module name precedes
	  python-identifier "[ \t]*")
  "Regular expression matching optional a single Python parent class.
Grouped expression 3 matches the parent class name.")

(defconst python-class-name-before  "^[ \t]*class[ \t\n\r]+"
  "Regexp preceding the class name in a class definition.  
Note: this does not allow for nested classes.")

(defconst python-class-name-after
  (concat "[ \t]*" python-parent-class-name "[ \t\n\r]*:")
  "Regexp following the class name in a class definition.")

(defconst python-class-def-regexp
  (concat python-class-name-before python-identifier "[ \t\n\r]*")
  "Regular expression used to match to class definitions in source text.
Class name identifier is grouped expression 1.  Parent identifier is grouped
expression 2.")


(defconst python-lang-prefix "python-"
 "Prefix string that starts \"br-python.el\" symbol names.")

(defconst python-src-file-regexp ".\\.py$"
  "Regular expression matching a unique part of Python source file name and no others.")

(defvar python-children-htable nil
  "Htable whose elements are of the form: (LIST-OF-CHILD-CLASSES . CLASS-NAME).
Used to traverse Python inheritance graph.  'br-build-children-htable' builds
this list.")
(defvar python-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Used to traverse Python inheritance graph.  'br-build-parents-htable' builds
this list.")
(defvar python-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
'br-build-paths-htable' builds this list.")


(defvar python-lib-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from stable software libraries are used to build the list.")
(defvar python-lib-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from stable software libraries are used to build the list.")

(defvar python-sys-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from systems that are likely to change are used to build the list.")
(defvar python-sys-paths-htable nil
  "Alist whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from systems that are likely to change are used to build the
list.")

(defvar python-lib-prev-search-dirs nil
  "Used to check if 'python-lib-classes-htable' must be regenerated.")
(defvar python-sys-prev-search-dirs nil
  "Used to check if 'python-sys-classes-htable' must be regenerated.")

(defvar python-env-spec nil
  "Non-nil value means Environment specification has been given but not yet built.
Nil means current Environment has been built, though it may still require updating.")

(provide 'br-python)
