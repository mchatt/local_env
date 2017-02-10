;;!emacs
;;
;; FILE:         br-java-ft.el
;; SUMMARY:      Java OO-Browser class and member functions.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     c, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:    01-Aug-95
;; LAST-MOD:     10-May-01 at 05:39:00 by Bob Weiner
;;
;; Copyright (C) 1995, 1996  BeOpen.com
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'br-c-ft)
(require 'br-java)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defconst java-default-interface-class "[interface]"
  "Name of the default class whose instances are Java interfaces.")

(defconst java-return-type-identifier
  (concat "\\([\[a-zA-Z][\]\[" java-return-type-chars "]*"
	  "[\]\[" java-return-type-chars "]+\\|[\[a-zA-Z]\\)"
	  "[ \t\n\r]*"))

(defconst java-type-identifier
  (concat "\\([\[a-zA-Z][\]\[" java-identifier-chars "]*[ \t\n\r]+\\)"))

(defconst java-type-tag-separator "@"
  "String that separates a tag's type from its normalized definition form.
This should be a single character which is unchanged when quoted for use as a
literal in a regular expression.")

(defconst java-tag-fields-regexp
  ;; The \\\\? below is necessary because we sometimes use this expression to
  ;; test against a string that has been regexp-quoted and some of the
  ;; characters in br-feature-type-regexp will then be preceded by \\.
  (format "^\\([^%s \n]+\\)%s\\\\?\\(%s \\)?\\([^%s\n]+\\)%s?"
	  java-type-tag-separator java-type-tag-separator
	  br-feature-type-regexp java-type-tag-separator
	  java-type-tag-separator)
 "Regexp matching the fields of a java feature tag line.
Group 1 is the class of the feature.  Optional group 2 is the prefix
preceding the feature when displayed within a listing buffer.  Group 3 is the
feature name.  The feature definition signature begins at the end of the
regexp match, i.e. (match-end 0), and goes to the end of the string or
line.")

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defconst java-code-file-regexp "\\.java?$"
  "Regular expression matching a unique part of a Java source file name and no others.")

(defconst java-type-def-modifier
  "\\(const\\|final\\|static\\|abstract\\|public\\|protected\\|private\\)")

(defconst java-type-modifier-keyword
  (concat
   "\\(\\(public\\|protected\\|private\\|const\\|abstract\\|"
   "synchronized\\|final\\|static\\|native\\|volatile\\)[ \t\n\r]+\\)"))

(defconst java-type-identifier-group
  (concat "\\(\\(" java-return-type-identifier "\\)[ \t\n\r]+\\)"))

(defconst java-function-identifier (concat
				    "[_a-zA-Z][^\]\[ \t\n\r\f:\;.,{}()=]*")
  "Regular expression matching a Java function name.")

(defconst java-arg-identifier
  (concat "[_a-zA-Z][" java-identifier-chars "]*")
  "Regular expression matching a Java function argument identifier.")

(defconst java-feature-decl-or-def
  (concat "\\(" java-type-modifier-keyword "*"
	  java-type-identifier-group "\\)?"
	  ;; Constructors don't use an explicit return type, so make this
	  ;; optional.
	  java-type-identifier "?"
	  "\\(" java-function-identifier "\\|" java-identifier "\\)"
	  ;; It is hard to tell arguments from parenthesized initializing
	  ;; expressions.
	  "[ \t\n\r]*\\(([^\)\;{}]*)\\)?\\([\]\[ \t]*\\)"
	  ;; Optional exceptions that a method can throw.
	  "\\([ \t\n\r]*\\<throws\\>[ \t\n\r]*\\("
	  java-identifier  "[, \t\n\r]*\\)+\\)?"
	  )
  "Regexp matching a java member declaration or definition.
Member modifier keywords are grouped expression `java-feature-mod-grpn'.
Member type is grouped expression `java-feature-type-grpn'.  Member name is
group `java-feature-name-grpn'.  Function parentheses, if any, are group
`java-feature-parens-grpn'.  Comma separated list of exceptions that can be
thrown by a function are group `java-feature-exceptions-grpn'.")

(defconst java-feature-mod-grpn 2)
(defconst java-feature-type-grpn 5)
(defconst java-feature-name-grpn 8)
(defconst java-feature-parens-grpn 10)
(defconst java-feature-exceptions-grpn 13)
(defconst java-feature-terminator-grpn 15)

(defconst java-at-feature-regexp
  (concat java-feature-decl-or-def
	  ;; Don't change this next expression to just optional whitespace
	  ;; since that can trigger a regexp stack overflow.
	  "\\([ \t\n\r\f]+\\|\\)")
  "See documentation of `java-feature-decl-or-def' for grouping expressions.")

(defconst java-feature-decl
  (concat java-at-feature-regexp "\\([=\;\{]\\)")
  "See documentation of `java-feature-decl-or-def' for grouping expressions.
`java-feature-terminator-grpn' holds the equal-sign, semi-color or opening brace
that triggers the end of the match.")

(defconst java-feature-decl-anchored
  (concat "^[ \t]*" java-at-feature-regexp "\\([=\;\{]\\)")
  "See documentation of `java-feature-decl-or-def' for grouping expressions.
`java-feature-terminator-grpn' holds the equal-sign, semi-color or opening brace
that triggers the end of the match.")

(defconst java-routine-def
  (concat java-at-feature-regexp "\\([\;\{]\\)")
  "See documentation of `java-feature-decl-or-def' for grouping expressions.
`java-feature-terminator-grpn' holds the opening brace that terminates the
feature declaration or the semi-colon that terminates native and abstract
method declarations.")

(defconst java-class-decl
  (concat java-class-modifier-keyword 
	  java-class-keyword java-identifier "[ \t]*[\;,]")
  "Regexp matching a java class declaration.
Class name is grouping `java-class-name-grpn'.")

(defconst java-class-name-grpn 4)

(defconst java-abstract-method-regexp "\\<abstract\\>[^\;{}]+\;"
  "Regexp matching a Java abstract method signature.")

(defconst java-native-method-regexp   "\\<native\\>[^\;{}]+\;"
  "Regexp matching a Java native method signature, one implemented in another language.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun java-add-default-classes ()
  (br-add-default-classes (list java-default-interface-class))
  (if br-c-tags-flag (c-add-default-classes)))

(defun java-member-p ()
  "Prints whether entity at point is a java member definition or declaration."
  (interactive)
  (let ((name))
    (save-excursion
      (message
       (concat
	"Is " (if (java-feature)
		  (progn (setq name
			       (br-buffer-substring (match-beginning
						     java-feature-name-grpn)
						    (match-end
						     java-feature-name-grpn)))
			 "")
		"not ")
	"a def.  "
	"Is " (if (and (java-skip-to-statement) (java-feature-decl))
		  (progn (setq name
			       (br-buffer-substring (match-beginning
						     java-feature-name-grpn)
						    (match-end
						     java-feature-name-grpn)))
			 "")
		"not ")
	"a member decl.  "
	(if name (concat "  Name = " name)))))))

(defun java-feature-implementors (name)
  "Return unsorted list of java feature tags which implement feature NAME.
This includes classes which declare abstract functions with NAME."
  ;; Eliminate matches to interface signatures (recognized by a leading `<' in
  ;; the class name field) since these are not implementations.
  (delq nil
	(mapcar (function (lambda (sig) (if (eq (aref sig 0) ?<) nil sig)))
		(java-feature-matches (concat "^" (regexp-quote name) "$")))))

(defun java-feature-locate-p (feature-tag &optional regexp-flag)
  "Leaves point at the start of FEATURE-TAG's definition in the current buffer.
Assumes caller has moved point to the beginning of the buffer or to the point
of desired search start.
Optional REGEXP-FLAG means FEATURE-TAG is a regular expression."
  ;; Match to function definitions, not declarations, except for abstract
  ;; methods which are declared, not defined, and so end with a `;'.
  (let ((case-fold-search) (found t) (class) start feature-sig feature-regexp)
    (if (br-feature-tag-p feature-tag)
	(setq feature-sig (br-feature-tag-signature feature-tag)
	      class (br-feature-tag-class feature-tag))
      (setq feature-sig feature-tag
	    class nil))
    (if regexp-flag
	(if (stringp feature-tag)
	    (setq feature-regexp feature-tag)
	  (error "(java-feature-locate-p): Not a regexp, %s" feature-tag)))
    ;;
    ;; First move to the proper class/interface declaration so that if two
    ;; classes in the same file have the same feature signature, we still end
    ;; up at the right one.
    (if (or class
	    (and (string-match java-tag-fields-regexp feature-sig)
		 (setq class (substring feature-sig
					(match-beginning 1) (match-end 1))
		       feature-sig (substring feature-sig (match-end 0)))))
	(while (and (re-search-forward
		     (java-class-definition-regexp
		      (if (equal class java-default-interface-class)
			  feature-sig
			class) regexp-flag)
		     nil t)
		    (setq start (match-beginning 0))
		    (not (setq found (not 
				      (if (c-within-comment-p)
					  (progn (search-forward "*/" nil t)
						 t))))))))
    ;;
    ;; If class was searched for and not found, or if this is an interface
    ;; declaration entry, then skip down to code display.
    (if (or (not found)
	    (equal class java-default-interface-class))
	nil
      ;; Otherwise, look for feature expression.
      (setq found nil)
      (or regexp-flag (setq feature-regexp
			    (java-feature-signature-to-regexp feature-sig)))
      (while (and (re-search-forward feature-regexp nil t)
		  (setq start (match-beginning 0))
		  (not (setq found (not
				    (if (c-within-comment-p)
					(progn (search-forward "*/" nil t)
					       t))))))))
    (if found (br-display-code start))))

(defun java-feature-name-to-regexp (name)
  "Converts routine NAME into a regular expression matching the routine's name tag."
  (java-feature-signature-to-regexp name))

(defun java-feature-signature-to-name (feature-sig-or-tag &optional with-class for-display)
  "Extracts the feature name from FEATURE-SIG-OR-TAG.
The feature's class name is dropped from FEATURE-SIG-OR-TAG unless optional
WITH-CLASS is non-nil.  If optional FOR-DISPLAY is non-nil, a feature type
character is prepended to the name for display in a browser listing."
  (if (br-feature-tag-p feature-sig-or-tag)
      (br-feature-tag-name feature-sig-or-tag with-class for-display)
    (let ((name))
      (cond
       ;; member
       ((string-match java-tag-fields-regexp feature-sig-or-tag)
	(setq name (substring feature-sig-or-tag
			      (match-beginning
			       (if (and for-display (match-beginning 2))
				   2 3))
			      (match-end 3)))
	(if with-class
	    (setq name (concat
			(substring feature-sig-or-tag
				   (match-beginning 1) (match-end 1))
			"::" name)))
	;; Remove any trailing whitespace.
	(br-delete-space name))
       ;;
       ;; unknown
       (t ;; Remove any trailing whitespace and add display prefix.
	(setq name (br-delete-space feature-sig-or-tag))
	(if for-display (java-feature-add-prefix
			 name "" feature-sig-or-tag) name))))))

(defun java-feature-signature-to-regexp (signature)
  "Given a java SIGNATURE, return regexp used to match to its definition."
  (setq signature (regexp-quote signature))
  (let ((prefix-info
	 (if (string-match java-tag-fields-regexp signature)
	     (prog1 (substring signature (match-beginning 0) (match-end 0))
	       (setq signature (substring signature (match-end 0)))))))
    (let ((pat) (i 0) (c) (len (length signature)))
      (while (< i len)
	(setq c (aref signature i)
	      pat (cond ((eq c ?\ )
			 ;; Allow for possible single line comment
			 ;; following any whitespace, e.g. following
			 ;; each routine argument.
			 (concat pat "[ \t\n\r]*\\(//.*\\)?"))
			(t
			 (concat pat (char-to-string c))))
	      i (1+ i)))
      (setq pat (concat prefix-info pat)))))

;;; The OO-Browser depends on the name of this next function; don't change it.
(defun java-list-protocols (class)
  "Return a sorted list of the parent interfaces of CLASS."
  (sort (delq nil (mapcar (function (lambda (parent)
				      (if (eq (aref parent 0) ?<)
					  parent)))
			  (br-get-parents class)))
	'string-lessp))

(defun java-to-definition (&optional other-win)
  "If point is within a declaration or identifier reference, try to move to its definition.
With OTHER-WIN non-nil, show it in another window."
  (interactive)
  (let ((opoint (point)))
    (cond
     ((java-feature other-win))
     ((and (goto-char opoint)
 	   (br-check-for-class (java-find-class-name) other-win)))
     ((br-check-for-class (java-class-decl-p) other-win))
     ;; Look it up as a regular tag to follow identifier references.
     ((and (goto-char opoint) (smart-java-tag))))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun java-class-decl-p ()
  "Return nil unless point is within a class declaration, referenced by another class.
Commented declarations also return nil.  When value is non-nil, it is
the class name from the declaration.  Leave point at the start of the statement for
visual clarity."
  (java-skip-to-statement)
  (save-excursion
    (let ((class))
      (and (looking-at java-class-decl)
	   (setq class (br-buffer-substring
			(match-beginning java-class-name-grpn)
			(match-end java-class-name-grpn)))
	   (not (c-within-comment-p))
	   (progn (beginning-of-line)
		  (not (looking-at "[ \t]*//")))
	   class))))

(defun java-feature (&optional other-win)
  "Move point to the definition of a member given by the declaration at point (other than an attribute).
Point must be within the declaration signature and not within its
brace-delimited body or this will simply return nil.
Return nil if point is anywhere other than within a member signature."
  (interactive)
  (let ((opoint (point)) modifier)
    (java-skip-to-statement)
    (if (and (= (point) (save-excursion (back-to-indentation) (point)))
	     (not (c-within-comment-p))
	     (save-excursion (beginning-of-line)
			     (not (looking-at "[ \t]*//")))
	     (not (java-feature-lookalikes))
	     (looking-at (concat java-at-feature-regexp "[=\;,\{]"))
	     ;; Ignore possible attributes.
	     (match-end java-feature-parens-grpn))
	(cond ((eq ?\{ (char-after (1- (match-end 0))))
	       (message "(OO-Browser):  Feature definition is at the top of the window.")
	       (recenter 0)
	       t)
	      ((and (match-beginning java-feature-mod-grpn)
		    (setq modifier
			  (br-buffer-substring
			   (match-beginning java-feature-mod-grpn)
			   (match-end java-feature-mod-grpn)))
		    ;; If this is a native or abstract method, alert user that
		    ;; its definition is elsewhere.
		    (cond ((string-match "\\<abstract\\>" modifier)
			   (message "(OO-Browser):  Abstract method, definition deferred to descendants")
			   t)
			  ((string-match "\\<native\\>" modifier)
			   (message "(OO-Browser):  Native method, defined in an external language")
			   t)))))
      (goto-char opoint)
      nil)))

(defun java-feature-add-prefix (feature-name class signature)
  "Add a browser listing display prefix to FEATURE-NAME from CLASS based on feature's SIGNATURE."
  (concat (cond
	   ;; interfaces
	   ((eq (aref signature 0) ?\<) nil)
	   ;; native methods
	   ((string-match java-native-method-regexp signature) "/ ")
	   ;; constructors and destructors
	   ((or (string-equal feature-name class)
		(string-equal feature-name "finalize")) "+ ")
	   ;; abstract methods
	   ((or (string-match java-abstract-method-regexp signature)
		;; Implicitly abstract method from an interface declaration 
		(string-match "\)\\s-*\;\\s-*\\'" signature)) "> ")
	   ;; attributes
	   ((string-match "\\`[^()]+[=\;,]\\'" signature) "= ")
	   ;; regular methods
	   (t "- "))
	  feature-name))

(defun java-feature-lookalikes ()
  (or (looking-at
       "\\(if\\|else if\\|else\\|for\\|while\\|switch\\|catch\\|synchronized\\)\\s-*\(")
      (looking-at java-class-decl)))

(defun java-feature-decl ()
  (and (not (java-feature-lookalikes))
       (looking-at java-feature-decl)))

(defun java-feature-map-tags (function regexp)
  "Apply FUNCTION to all current feature tags that match REGEXP and return a list of the results."
  (let ((identifier-chars (concat "[" java-identifier-chars "]*")))
    ;; Ensure handle "^" and "$" meta-chars.
    (setq regexp
	  (concat (format "\\`%s " br-feature-type-regexp)
		  (if (equal (substring regexp 0 1) "^")
		      (progn (setq regexp (substring regexp 1)) nil)
		    identifier-chars)
		  (if (equal (substring regexp -1) "$")
		      (substring regexp 0 -1)
		    (concat regexp identifier-chars))
		  "\\'"))
    (br-feature-map-tags function regexp)))

(defun java-feature-matches (regexp)
  "Return an unsorted list of feature tags whose names match in part or whole to REGEXP.
^ and $ characters may be used to match to the beginning and end of a feature name,
respectively."
  (java-feature-map-tags 'identity regexp))

(defun java-feature-normalize (feature class name)
  "Return a feature tag based on FEATURE, CLASS and NAME."
  (setq class (br-delete-space class)
	name (java-feature-add-prefix name class feature))
  (concat class java-type-tag-separator
	  name java-type-tag-separator 
	  (br-feature-delete-c-comments feature)))

(defun java-files-with-source (class)
  "Use CLASS to compute set of files that match to a java source file regexp.
Return as a list."
  (let ((file (if class (br-class-path class) buffer-file-name)))
    (and file
	 (let* ((src-file-regexp (concat "^" (br-filename-head file)
					 java-code-file-regexp))
		(dir (file-name-directory file))
		(files (directory-files dir nil src-file-regexp)))
	   (mapcar (function (lambda (f) (expand-file-name f dir)))
		   files)))))

(defun java-find-ancestors-feature (class-list ftr-pat &optional other-win)
  "Scan ancestors of CLASS-LIST and show routine definition matching FTR-PAT."
  ;; If no class, search for non-member function.
  (or class-list (setq class-list '(nil)))
  (br-feature-display class-list ftr-pat other-win))

(defun java-find-class-name ()
  "Return current word as a potential class name."
  (save-excursion
    (let* ((start)
	   (ignore "\]\[ \t\n\r\f\;,.\(\){}-")
	   (pat (concat "^" ignore)))
      (forward-char 1)
      (skip-chars-backward ignore)
      (skip-chars-backward pat)
      (setq start (point))
      (skip-chars-forward pat)
      (br-buffer-substring start (point)))))

(defun java-get-class-name-from-source ()
  "Return class name from closest class definition preceding point or nil."
  (let ((opoint (point))
	(class))
    (save-excursion
      (if (re-search-backward java-class-def-regexp nil t)
	  (progn (goto-char (match-beginning java-class-def-derived-grpn))
		 (setq class (java-normalize-class-match))
		 ;; Ensure that declaration occurs within class definition.
		 (forward-list)
		 (and (> (point) opoint)
		      class))))))

(defun java-output-feature-tags (feature-file feature-tags-list)
  "Write Java FEATURE-FILE's FEATURE-TAGS-LIST into `br-feature-tags-file'.
Assume `br-feature-tags-init' has been called."
  (interactive)
  (save-excursion
    (br-feature-set-tags-buffer)
    (goto-char 1)
    ;; Delete any prior feature tags associated with feature-file
    (if (search-forward feature-file nil 'end)
	(progn (forward-line -1)
	       (let ((start (point)))
		 (search-forward "\^L" nil 'end 2)
		 (backward-char 1)
		 (delete-region start (point)))))
    (if feature-tags-list
	(progn (insert "\^L\n")
	       ;; Quote pathname to avoid read errors on MS OSes.
	       (prin1 feature-file (current-buffer))
	       (insert "\n")
	       (mapcar (function (lambda (tag) (insert tag "\n")))
		       feature-tags-list)))))

(defun java-scan-ancestors-feature (class-list ftr-pat &optional other-win)
  "Display feature definition derived from CLASS-LIST, matching FTR-PAT.
Scan files with same base name as class file."
  (let  ((classes class-list)
	 (found-ftr)
	 (code-def-files)
	 (file)
	 (ftr-regexp)
	 (class))
    (if (null class-list)
	nil
      (while (and (not found-ftr) classes)
	(setq class (car classes)
	      code-def-files (java-files-with-source class)
	      ftr-regexp (funcall ftr-pat class))
	(while (and (setq file (car code-def-files))
		    (not (setq found-ftr
			       (br-feature-found-p file ftr-regexp
						   nil other-win t))))
	  (setq code-def-files (cdr code-def-files)))
	(setq classes (if found-ftr nil (cdr classes))))
      (if found-ftr
	  (or class t)
	(java-scan-ancestors-feature
	 (apply 'append (mapcar (function (lambda (cl) (br-get-parents cl)))
				class-list))
	 ftr-pat)))))

(defun java-scan-features (class start end)
  "Return reverse ordered list of java feature declarations within CLASS declaration.
START and END give buffer region to search.

Multiple declarations with only one type, e.g. float a, b;
are missed, because that would require too much effort right now.
Use the clearer style with a type keyword for each feature defined."
  (setq class (br-delete-space class))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (let ((feature-list) ftr name)
	;;
	;; Get member definitions and abstract method declarations.
	(while (re-search-forward java-feature-decl-anchored nil t)
	  (setq start (match-beginning 0)
		name  (br-buffer-substring
		       (match-beginning java-feature-name-grpn)
		       (match-end java-feature-name-grpn))
		ftr  (br-buffer-substring (match-beginning 0) (match-end 0)))
	  ;; This is necessary to remove a possible double expression match
	  ;; where there is a blank line within the match.
	  (if (string-match "\n\\([ \t]*\r?\n\\)+" ftr)
	      (progn (setq ftr (substring ftr (match-end 0)))
		     (goto-char (+ start (match-end 0))))
	    (if (c-within-comment-p)
		(search-forward "*/" nil t)
	      ;; Move point to precede the feature match termination character.
	      (backward-char)
	      (cond ((eq (following-char) ?\{)
		     (condition-case ()
			 ;; Move to end of feature but ignore any error if braces
			 ;; are unbalanced.  Let the compiler tell the user about
			 ;; this.
			 (forward-sexp)
		       (error nil)))
		    ((eq (following-char) ?=)
		     (skip-chars-forward "^\;")))
	      (setq ftr (java-feature-normalize ftr class name)
		    feature-list (cons ftr feature-list)))))
	feature-list))))

(defun java-skip-to-statement ()
  (if (re-search-backward "\\(^\\|[\;{}()]\\)[ \t]*" nil t)
      (progn (goto-char (match-end 0))
	     (skip-chars-forward " \t")
	     t)))

(provide 'br-java-ft)
