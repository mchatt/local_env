;;!emacs
;;
;; FILE:         br-c++.el
;; SUMMARY:      Support routines for C++ inheritance browsing.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     c, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:     7-Dec-89
;; LAST-MOD:      9-Jun-99 at 18:03:29 by Bob Weiner
;;
;; Copyright (C) 1990-1999  BeOpen.com
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(mapcar 'require '(br-lib hypb br-c-ft))

;;; ************************************************************************
;;; User visible variables
;;; ************************************************************************

(defconst c++-class-name-modifier
  "\\([_a-zA-Z][_a-zA-Z0-9]*[ \t\n\r]+\\)?"
  "Regexp for an optional #define keyword preceding the name of a class within a class declaration.
Some class libraries use this technique.")

(defvar c++-class-keyword
  (concat "\\(class\\|struct\\|union\\)[ \t\n\r]+"
	  c++-class-name-modifier)
  "*Keyword regexp preceding a C++ class declaration or definition.")

(defvar   c++-lib-search-dirs nil
  "List of directories below which C++ Library source files are found.
Subdirectories of Library source are also searched.  A Library is a stable
group of classes.")

(defvar   c++-sys-search-dirs nil
  "List of directories below which C++ System source files are found.
Subdirectories of System source are also searched.  A System class is one
that is not yet reusable and is likely to change before release.")


(defconst c++-narrow-view-to-class nil
 "*Non-nil means narrow buffer to just the matching class definition when displayed.")

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(defun c++-get-classes-from-source (filename &optional skip-tags
				    skip-tags-cleanup)
  "Scans FILENAME and returns cons of class list with parents-class alist.
Handles multiple inheritance.  Assumes file existence and readability have
already been checked.
   With optional SKIP-TAGS non-nil, does not compute and store lookup tags
for member definitions.  If SKIP-TAGS is nil, normally a cleanup
function is called after scanning the members.  SKIP-TAGS-CLEANUP
non-nil suppresses this action."
  (let (class-name-end classes class has-parents open-brace-point
	parents parent-cons signatures)
    (funcall br-view-file-function filename)
    (setq buffer-read-only nil)
    (save-excursion
      (save-restriction
	(widen)
	(br-buffer-delete-c-comments)
	(goto-char (point-min))
	(or skip-tags
	    (progn (setq signatures (c++-scan-features))
		   (goto-char (point-min))))
	(while (re-search-forward c++-class-def-regexp nil t)
	  (setq has-parents
		(= ?: (char-after
		       (match-beginning c++-class-def-derived-grpn)))
		class-name-end (match-end c++-class-def-name-grpn)
		;;
		;; Now since we've saved all the match expressions we need
		;; from our last regexp match, we can call functions which
		;; change the match data below here.
		class (c++-normalize-class-match t)
		parent-cons (cons (if has-parents
				      ;; Return parents as a list.
				      (c++-scan-parents))
				  class))
	  (setq classes (cons class classes)
		parents (cons parent-cons parents))
	  (or skip-tags
	      ;; Scan members defined within class
	      (progn (goto-char class-name-end)
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
					   (c++-scan-features-in-class
					    class open-brace-point
					    (point))))))))))))
    (if skip-tags
	nil
      (c++-output-feature-tags filename signatures)
      (or skip-tags-cleanup (br-feature-build-htables)))
    (set-buffer-modified-p nil)
    (cons classes (delq nil parents))))

(defun c++-class-definition-regexp (class &optional regexp-flag)
  "Return regexp to uniquely match the definition of CLASS name.
Optional REGEXP-FLAG non-nil means CLASS has already been quoted for use in a
regular expression."
  (let ((template-args-regexp (c++-template-args-regexp class)))
    (concat "^[ \t]*"
	    (if template-args-regexp
		;; Only match to a class definition with the same number of
		;; template parameters as <class> since some modules use #ifdef
		;; to define classes with the same name but a different number
		;; of template parameters.
		(format "\\(template[ \t\n\r]*%s[ \t\n\r]*\\)"
			template-args-regexp))
	    c++-class-keyword
	    c++-class-name-modifier
	    (if regexp-flag
		(c++-class-non-template-name class)
	      (regexp-quote (c++-class-non-template-name class)))
	    c++-class-name-after)))

(defun c++-template-args-regexp (class)
  "Return a regexp matching the number of template args in CLASS or nil when there are no such arguments."
  (if (string-match "\<[^!]+\>\\'" class)
      (let* ((param "[^,<>]+")
	     (comma (concat "," param)))
	(format "<%s%s>"
		param (mapconcat
		       (function (lambda (c) (if (= c ?\,) comma)))
		       (substring class (1+ (match-beginning 0))
				  (1- (match-end 0)))
		       "")))))

;; Remove only *trailing* template identifiers when class name is looked up.
(defun c++-class-non-template-name (class)
  "Return CLASS name sans any trailing <template> component.
Does not remove whitespace from CLASS."
  (if (and (stringp class) (string-match "\<[^!]+\>\\'" class))
      (substring class 0 (match-beginning 0))
    class))

(defun c++-get-class-name (class-name template-signature rename-arguments-flag)
  "Return a possibly parameterized class identifier built from CLASS-NAME and TEMPLATE-SIGNATURE.
If RENAME-ARGUMENTS-FLAG is non-nil, template class argument names are
normalized also to T1,T2,T3, etc.
TEMPLATE-SIGNATURE may be of any of the following forms:
   nil                              =>  class-name
   template <class T>               =>  class-name<T>
   template <class T1, class T2>    =>  class-name<T1,T2>
   <class T1, class T2>             =>  class-name<T1,T2>
   <int = 0>                        =>  class-name<int>
   <int size = 0>                   =>  class-name<size>."
  (cond ((null template-signature)
	 class-name)
	((stringp template-signature)
	 (setq template-signature
	       (if rename-arguments-flag
		   (progn
		     ;; Remove any text prior to template arguments.
		     (if (string-match "\<" template-signature)
			 (setq template-signature
			       (substring template-signature
					  (match-beginning 0))))
		     (c++-normalize-template-arguments template-signature))
		 (c++-template-argument-names template-signature)))
	 (if (null template-signature)
	     class-name
	   (setq class-name
		 (format "%s%s" class-name template-signature))))
	(t (error "(c++-get-class-name): Second argument, `%s', must be a string or nil."
		  template-signature))))

(defun c++-template-argument-names (template-signature)
  "Return a delimited string of the template argument names from TEMPLATE-SIGNATURE."
  (if (or (null template-signature)
	  (not (string-match "\<" template-signature)))
      ;; No type parameters.
      nil
    (setq template-signature (br-delete-space template-signature))
    (let ((depth 0) (args) (arg ""))
      (mapcar
       (function
	(lambda (c)
	  (cond ((eq c ?\<)
		 (setq depth (1+ depth))
		 (cond ((= depth 1)
			(setq args (concat args "\<")))
		       ((> depth 1)
			(setq arg (concat arg (char-to-string c))))))
		((zerop depth))
		((= depth 1)
		 (cond ((memq c '(?, ?\>))
			(if (string-match
			     c++-template-parameter-regexp arg)
			    (setq arg (substring arg
				       (match-beginning
					c++-template-parameter-grpn)
				       (match-end
					c++-template-parameter-grpn))))
			(setq args (concat args (br-delete-space arg)
					   (char-to-string c))
			      arg "")
			(if (eq c ?\>) (setq depth (1- depth))))
		       (t (setq arg (concat arg (char-to-string c))))))
		((eq c ?\>)
		 (setq depth (1- depth)
		       arg (concat arg (char-to-string c))))
		(t (setq arg (concat arg (char-to-string c)))))))
       template-signature)
      args)))

(defun c++-list-template-arguments (template-signature)
  "Return a list of the template arguments within TEMPLATE-SIGNATURE."
  (if (or (null template-signature)
	  (not (string-match "\<" template-signature)))
      ;; No type parameters.
      nil
    (setq template-signature (br-delete-space template-signature))
    (let ((depth 0) (args) (arg ""))
      (mapcar
       (function
	(lambda (c)
	  (cond ((eq c ?\<)
		 (setq depth (1+ depth))
		 (if (> depth 1)
		     (setq arg (concat arg (char-to-string c)))))
		((zerop depth))
		((= depth 1)
		 (cond ((eq c ?,)
			(setq args (cons (br-delete-space arg) args)
			      arg ""))
		       ((eq c ?\>)
			(setq args (cons (br-delete-space arg) args)
			      arg ""
			      depth (1- depth)))
		       (t (setq arg (concat arg (char-to-string c))))))
		((eq c ?\>)
		 (setq depth (1- depth)
		       arg (concat arg (char-to-string c))))
		(t (setq arg (concat arg (char-to-string c)))))))
       template-signature)
      (nreverse args))))

(defun c++-normalize-class-match (rename-arguments-flag)
  "After a regexp match to a class definition, return the matching class name.
Class name is normalized for use in OO-Browser lookups.
If RENAME-ARGUMENTS-FLAG is non-nil, template class argument names are
normalized also to T1,T2,T3, etc."
 (c++-get-class-name
  (br-buffer-substring (match-beginning c++-class-def-name-grpn)
		       (match-end c++-class-def-name-grpn))
  (if (match-beginning c++-class-def-template-grpn)
      (br-buffer-substring
       (match-beginning c++-class-def-template-grpn)
       (match-end c++-class-def-template-grpn)))
  rename-arguments-flag))

(defun c++-normalize-template-arguments (class)
  "Return CLASS with any template arguments renamed to <T> or <T1,T2,T3>."
  (cond ((null class) nil)
	((not (string-match "\<" class))
	 ;; No type parameters.
	 (br-delete-space class))
	(t (setq class (br-delete-space class))
	   (let ((depth 0) (arg-num 0))
	     (mapconcat
	      (function
	       (lambda (c)
		 (cond ((eq c ?<)
			(setq depth (1+ depth))
			(if (= depth 1) "<"))
		       ((zerop depth) (char-to-string c))
		       ((= depth 1)
			(cond ((eq c ?,)
			       (setq arg-num (1+ arg-num))
			       (format "T%d," arg-num))
			      ((eq c ?>)
			       (setq arg-num (1+ arg-num)
				     depth (1- depth))
			       (if (= arg-num 1)
				   "T>"
				 (format "T%d>" arg-num)))))
		       ((eq c ?>)
			(setq depth (1- depth))
			nil))))
	      class
	      "")))))

(defun c++-scan-parents ()
  "Return list of parents names from a C++ class definition.
Point must be after the colon that begins the parent list and before the
first parent entry when this function is called."
  (let ((parent-list) (again t)
	parent)
    (while (and again (re-search-forward c++-parent-regexp nil t))
      (setq parent
	    (c++-parse-buffer-template-arguments
	     (br-delete-space
	      (br-buffer-substring (match-beginning c++-parent-name-grpn)
				   (match-end c++-parent-name-grpn)))))
      (if (looking-at (concat c++-comment-regexp "[#,\{\;]"))
	  (goto-char (match-end 0)))
      (setq again (eq ?, (preceding-char))
	    parent-list (cons parent parent-list)))
    (nreverse parent-list)))

(defun c++-parse-buffer-template-arguments (class)
  "Return CLASS with any template arguments following point renamed to <T> or <T1,T2,T3>.
Leaves point after the closing angle bracket."
;; We need to handle class definitions like this:
;;   template <class T> class PtrList : private List<type-name> {}
;; where the parent class is an instantiation of a parameterized class.
;; For now, we change the type name to <T> or <T1,T2,T3> when there are 3
;; parameters, for purposes of class name matching.
;;	
;; Test cases:
;;
;;    '("par <class _T1=myclass , class _T2 = you >" "parent"
;;	"class<_T1,T2>" "class< __me , int>" "" "<template>"
;;      "par<_template>")
;;   Should yield:
;;     ("par<T1,T2>" "parent" "class<T1,T2>" "class<T1,T2>" "" "<template>"
;;      "par<T>")
;;
  (if (not (eq (following-char) ?\<))
      class
    (let ((depth 1) (arg-num 0) (args "\<")
	  (continue t) c)
      (while continue
	(forward-char 1)
	(setq c (following-char))
	(cond ((eq c ?<)
	       (setq depth (1+ depth)))
	      ((= depth 1)
	       (cond ((eq c ?,)
		      (setq arg-num (1+ arg-num)
			    args (concat args (format "T%d," arg-num))))
		     ((eq c ?>)
		      (setq arg-num (1+ arg-num)
			    depth (1- depth)
			    continue nil)
		      (setq args (concat args
					 (if (= arg-num 1)
					     "T>"
					   (format "T%d>" arg-num)))))))
	      ((eq c ?>)
	       (setq depth (1- depth)))
	      ((zerop depth)
	       (setq continue nil))))
      ;; Move past closing angle bracket.
      (forward-char 1)
      ;; Return class with normalized args.
      (concat class args))))

(defun c++-get-parents-from-source (filename class-name)
  "Scan source in FILENAME and return list of parents of CLASS-NAME.
Assume file existence has already been checked."
    (or (null class-name)
	(car (car (br-rassoc
		   class-name
		   (cdr (c++-get-classes-from-source filename t)))))))

(defun c++-select-path (paths-htable-elt &optional feature-p)
  "Select proper pathname from PATHS-HTABLE-ELT based upon value of optional FEATURE-P.
Selection is between path of class definition and path for features associated
with the class."
  (let ((elt (cdr paths-htable-elt)))
    (if (consp elt) 
	(if feature-p (cdr elt) (car elt))
      ;; Both paths are the same.
      elt)))

(defun c++-set-case (type)
  "Return string TYPE identifier for use as a class name."
  type)

(defun c++-set-case-type (class-name)
  "Return string CLASS-NAME for use as a type identifier."
  class-name)

(defun c++-to-class-end ()
  "Assuming point is at start of class, move to start of line after end of class."
  (interactive)
  (condition-case ()
      (forward-list)
    (error (progn (or (re-search-forward "^}" nil t)
		      (goto-char (point-max))))))
  (forward-line 1))

(defalias 'c++-to-comments-begin 'br-c-to-comments-begin)

;;; ************************************************************************
;;; Internal variables
;;; ************************************************************************

(defconst c++-template-prefix
  "\\(template[ \t\n\r]*\<[^\>\;.{}]+\>[ \t\n\r]*\\)?"
  "Regexp matching a template class or element definition or declaration.
Entire expression is an optional match, so it may be added as a conditional
expression to other regexps.")

(defconst c++-class-name-before
  (concat "^[ \t]*" c++-template-prefix c++-class-keyword)
  "Regexp preceding the class name in a class definition.")

(defconst c++-comment-regexp "\\([ \t\n\r]*//.*[\n\r]\\|[ \t\n\r]*/\\*.*\\*/\\)*[ \t\n\r]*")

(defconst c++-class-name-after
  (concat c++-comment-regexp "\\([\{:]\\)")
  "Regexp following the class name in a class definition.
Last character matched is either the colon preceding the list of class
parents, or the curly brace beginning the class body definition.")

(defconst c++-identifier-chars "_~<>a-zA-Z0-9"
  "String of chars and char ranges that may be used within a C++ or G++ identifier.")

(defconst c++-template-identifier-chars "_a-zA-Z0-9"
  "String of chars and char ranges that may be used within a standard C++ template identifier.
This excludes the template arguments.")

(defconst c++-return-type-chars "_<>a-zA-Z0-9"
  "String of chars and char ranges that may be used within a C++ or G++ return type identifier.")

;; Modified on 3/28/95 to handle C++ names with multiple template
;; parameters, e.g. class<T1,T2,T3>.
(defconst c++-identifier (concat
			  "\\([_~\<a-zA-Z][" c++-template-identifier-chars "]*"
			  "[ \t\n\r]*\<[^\>\;{}]+[ \t\n\r\>]*\>\\|[_~\<a-zA-Z]["
			  c++-identifier-chars "]*\\)")
  "Regular expression matching a C++ or G++ identifier.")

(defconst c++-class-def-regexp
  (concat c++-class-name-before c++-identifier c++-class-name-after)
  "Regular expression used to match to class definitions in source text.
Class name identifier is grouping `c++-class-def-name-grpn'.  Optional
class template parameter signature is grouping `c++-class-def-template-grpn'.
`:' derived class indicator begins grouping `c++-class-def-derived-grpn,'
unless the class is not derived, in which case this grouping begins with
`{'.")

(defconst c++-class-def-template-grpn 1)
(defconst c++-class-def-name-grpn 4)
(defconst c++-class-def-derived-grpn 6)

(defconst c++-lang-prefix "c++-"
 "Prefix string that starts \"br-c++.el\" symbol names.")

(defconst c++-non-template-identifier
  (concat "\\([_~\<a-zA-Z][" c++-template-identifier-chars "]*"
	  "[ \t\n\r]*\\|[_~\<a-zA-Z][" c++-identifier-chars "]*\\)")
  "Match a C++ identifier up until the start of any template arguments.")

(defconst c++-parent-regexp
  (concat c++-comment-regexp
	  "\\(\\(public\\|private\\|protected\\|virtual\\)[ \t\n\r]+"
	  "\\(\\(public\\|private\\|protected\\|virtual\\)[ \t\n\r]+\\)?\\)?"
	  ;; match c++-identifier up until start of any template arguments
	  c++-non-template-identifier)
  "Parent identifier (sans any template arguments) is group `c++-parent-name-grpn'.")

(defconst c++-parent-name-grpn 6)

(defconst c++-template-parameter-regexp
  "\\([^= \t\n\r]+\\)[ \t\n\r]*\\(=[^,\>]+\\)?\\'"
  "Regexp matching a single C++ <template> specifier argument name within a
single template argument.  For example in `class T', T is the parameter name
and in `int size = 0', size is the parameter name.  The parameter name is
grouping `c++-template-parameter-grpn'.")

(defconst c++-template-parameter-grpn 1)

;; Ellemtel C++ recommendations specify that inline definition files should
;; use the suffix ".icc" and other people use ".I" for such files, so those
;; suffixes are included here.
(defconst c++-src-file-regexp
  "[^.]\\.\\([chCH][xX][xX]\\|[chCH][chpCHP]?[pP]?\\|[iI][cC][cC]\\|I\\|[iI][nN][cC]\\)$"
  "Regular expression matching a unique part of C++ source or header file name and no others.")

(defconst c++-header-file-regexp
  "\\.\\([hH][xX][xX]\\|[hH][hpHP]?[pP]?\\|[iI][nN][cC]\\)$"
  "Regular expression matching the . and suffix of a C++ or C header file.")

(defvar c++-children-htable nil
  "Htable whose elements are of the form: (LIST-OF-CHILD-CLASSES . CLASS-NAME).
Used to traverse C++ inheritance graph.  `br-build-children-htable' builds
this list.")
(defvar c++-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Used to traverse C++ inheritance graph.  `br-build-parents-htable' builds
this list.")
(defvar c++-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
`br-build-paths-htable' builds this list.")


(defvar c++-lib-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from stable software libraries are used to build the list.")
(defvar c++-lib-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from stable software libraries are used to build the list.")

(defvar c++-sys-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from systems that are likely to change are used to build the list.")
(defvar c++-sys-paths-htable nil
  "Alist whose elements are of the form: (LIST-OF-CLASS-NAMES . FILE-PATH).
FILE-PATH gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from systems that are likely to change are used to build the
list.")

(defvar c++-lib-prev-search-dirs nil
  "Used to check if `c++-lib-classes-htable' must be regenerated.")
(defvar c++-sys-prev-search-dirs nil
  "Used to check if `c++-sys-classes-htable' must be regenerated.")

(defvar c++-env-spec nil
  "Non-nil value means Environment specification has been given but not yet built.
Nil means current Environment has been built, though it may still require updating.")

(provide 'br-c++)
