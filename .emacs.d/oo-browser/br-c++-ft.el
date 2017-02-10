;;!emacs
;;
;; FILE:         br-c++-ft.el
;; SUMMARY:      C++ OO-Browser class and member functions.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     c, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:    03-Oct-90
;; LAST-MOD:     10-May-01 at 03:02:42 by Bob Weiner
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

(require 'br-c++)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar c++-cpp-include-dirs '("/usr/include/")
  "*Ordered list of include directories by default searched by C preprocessor.
Each directory must end with a directory separator.  See also
`c++-include-dirs'.")

(defvar c++-include-dirs nil
  "*Ordered list of directories to search for C++ include files.
Each directory must end with a directory separator.  Directories normally
searched by the C++ pre-processor should be set instead in
`c++-cpp-include-dirs'.")

;; Modified on 3/28/95 to handle C++ names with multiple template
;; parameters, e.g. class<T1,T2,T3>.  Unfortunately for our pattern matcher,
;; we also have to allow spaces within the template parameters section.  We
;; consciously do not allow newlines within the parameters section to avoid
;; grabbing too much of the expression that follows.
(defconst c++-return-type-identifier
  (concat "[\[_\<a-zA-Z]"
	  "[\]" c++-template-identifier-chars "]*"
	  "[ \t\n\r]*\\(::[ \t\n\r]*[\]" c++-template-identifier-chars "]+\\)?"
	  "[ \t\n\r]*\<[" c++-return-type-chars " ,]+\>[ \t\n\r]*[*&]*"
	  "\\|[\[_\<a-zA-Z][\]" c++-return-type-chars "]*"
	  "\\([ \t\n\r]*::[ \t\n\r]*[\[\<a-zA-Z][\]" c++-return-type-chars "]+\\)?"
	  "[ \t\n\r]*[*&]*"))

(defconst c++-type-identifier
  (concat "\\([ \t\n\r]*::\\|[\[_\<a-zA-Z][\]"
	  c++-template-identifier-chars "]*"
	  "[ \t\n\r]*\<[^\>\;{}]+\>[ \t\n\r]*[*&]*[ \t\n\r]*::"
	  "\\|[\[_\<a-zA-Z][\]" c++-identifier-chars
	  "]*[ \t\n\r]*[*&]*[ \t\n\r]*::\\)"))

(defconst c++-type-tag-separator "@"
  "String that separates a tag's type from its normalized definition form.
This should be a single character which is unchanged when quoted for use as a
literal in a regular expression.")

(defconst c++-tag-fields-regexp
  ;; The \\\\? below is necessary because we sometimes use this expression to
  ;; test against a string that has been regexp-quoted and some of the
  ;; characters in br-feature-type-regexp will then be preceded by \\.
  (format "^\\([^%s\n]+\\)%s\\\\?\\(%s \\)\\([^%s\n]+\\)%s"
	  c++-type-tag-separator c++-type-tag-separator br-feature-type-regexp
	  c++-type-tag-separator c++-type-tag-separator)
 "Regexp matching the fields of a C++ feature tag line.
Group 1 is the class of the feature.  Group 2 is the prefix preceding the
feature when displayed within a listing buffer.  Group 3 is the feature name.
The feature definition signature begins at the end of the regexp match,
i.e. (match-end 0), and goes to the end of the string or line.")

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defconst c++-code-file-regexp "\\.\\(cxx\\|[cC][cC]?P?\\)$"
  "Regular expression matching a unique part of C++ source (non-header) file name and no others.")

(defconst c++-include-regexp
  "[ \t/*]*#[ \t]*include[ \t]+\\([\"\<]\\)\\([^\"\>]+\\)[\"\>]"
  "Regexp to match to C++ include file lines.  File name is grouping 2.  Type
of include, user-specified via double quote, or system-related starting with
`\<' is given by grouping 1.")

(defvar   c++-tmp-buffer-name (concat br-buffer-prefix-info "Tmp*")
  "Name of the buffer for short-term OO-Browser scratch work.")

(defconst c++-type-def-modifier
  "\\(auto\\|const\\|inline\\|mutable\\|register\\|static\\|typedef\\|unsigned\\)")

(defconst c++-type-modifier-keyword
  (concat "\\(\\(auto\\|const\\|explicit\\|extern[ \t\n\r]+\"[^\"]+\"\\|"
	  "extern\\|friend\\|inline\\|mutable\\|overload\\|"
	  "register\\|static\\|typedef\\|typename\\|unsigned\\|virtual\\)"
	  "[ \t\n\r]+\\)"))

(defconst c++-routine-modifier-keyword
  (concat
   "\\(\\)" ;; placeholder to align with c++-attribute-modifier-keyword
   "\\(\\([ \t\n\r]+const\\|[ \t\n\r]+mutable\\|[ \t\n\r]+restrict\\)?"
   "\\([ \t\n\r]*:[^\;\{]+\\|[ \t\n\r]*=[ \t]*0\\)?\\)"))

(defconst c++-type-identifier-group
  ;; It is critical that the final part of this expression, [*& \t\n\r]+,
  ;; stay exactly as it is or certain feature definitions may be missed or
  ;; segmented improperly.
  ;; If you remove the `*&', "Int &operator=(int j) {}", will not be found
  ;; because the & will be missed.  If you change the `+' to a `*', "main()"
  ;; will show up as "- n" in listing buffers.
  (concat "\\(\\(" c++-return-type-identifier "\\)[*& \t\n\r]+\\)"))

;; Final optional expression is to handle new C++ array operators, e.g.
;;    void operator delete [] (void*);
(defconst c++-operator-identifier "operator[ \t\n\r]*[^ \t\n\r\f:\;.,?~{}]+\\([ \t\n\r]*\\[\\]\\)?"
  "Regular expression matching a C++ or G++ operator name.")

(defconst c++-feature-decl-or-def
  (concat c++-template-prefix
	  "\\(\\(" c++-type-modifier-keyword "*\\)"
	  c++-type-identifier-group "\\)?"
	  "\\(" c++-type-identifier "[ \t\n\r]*\\)?"
	  "\\(" c++-operator-identifier "\\|" c++-function-identifier "\\|"
	  "[*&]?" c++-identifier "\\)" "[ \t\n\r]*")
  "Regexp matching the first part of a C++ method declaration or definition.")

(defconst c++-attribute-modifier-keyword
  (concat
   ;; Allow for array brackets, [...]
   "\\(\\[[\]\[_:~<>a-zA-Z0-9]*\\][ \t\n\r]*\\)*"
   "\\(\\([ \t\n\r]+const\\|[ \t\n\r]+mutable\\|[ \t\n\r]+restrict\\)?"
   "[ \t\n\r]*\\(=[^\;\{]+\\)?\\)"))

;; Empty groupings are for group numbering compatibility with
;; `c++-at-feature-regexp'.
(defconst c++-attribute-tag-regexp
  (concat "^[ \t]*" c++-template-prefix
	  "\\(\\(" c++-type-modifier-keyword "*\\)"
	  c++-type-identifier-group "\\)?"
	  "\\(\\)\\(\\)"
	  "\\(\\(\\)[*&]?" c++-identifier "\\)"
	  "[ \t\n\r]*"
	  c++-comment-regexp
	  "\\(" c++-attribute-modifier-keyword "?\\)"
	  "\\(\\)" ;; placeholder to align with `c++-comment-regexp' in
		   ;; `c++-at-feature-regexp'
	  ))

(defconst c++-routine-def-terminator-regexp
  "\\(\{\\)")

(defconst c++-feature-decl-terminator-regexp
  "\\(\;\\)")

(defconst c++-feature-def-terminator-regexp
  "\\([\{\;]\\)")

(defconst c++-attribute-decl
  ;; Must use `c++-feature-def-terminator-regexp' here since need to match to 
  ;; constructs like: enum ENUM_NAME {} ENUM_VAR;
  (concat c++-attribute-tag-regexp c++-feature-def-terminator-regexp)
  "Regexp matching a C++ attribute declaration within a class declaration.
Member modifier keywords are grouped expression `c++-feature-mod-grpn'.
Member type is grouped expression `c++-feature-type-grpn', unless scoping
type name, grouped expression `c++-feature-scope-grpn' is non-nil, in which
case, grouping `c++-feature-scope-grpn' is the type plus \"::\". 
Member name is group `c++-feature-name-grpn'.  Any text following the
attribute name but preceding the terminating semicolon is group
`c++-feature-post-args-grpn'.  Optional modifier keyword following the arguments
is group `c++-feature-post-modifier-grpn'.  Group `c++-feature-defaults-grpn'
optionally matches the default value of the attribute (including the preceding
equal sign).")

(defconst c++-feature-mod-grpn 3)
(defconst c++-feature-type-grpn 6)
(defconst c++-feature-scope-grpn 11)
(defconst c++-feature-name-grpn 12)
(defconst c++-feature-parens-grpn 16)
(defconst c++-feature-post-args-grpn 17)
;; Optional array square brackets is group 18.
(defconst c++-feature-post-modifier-grpn 19)
(defconst c++-feature-defaults-grpn 20)
;;
;; Match the attribute value including the leading =.
(defconst c++-attribute-value-grpn 21) 
;; Match the `c++-routine-in-class' or
;; `c++-routine-def-in-class' terminating semicolon, opening brace or
;; = 0 for pure virtual functions.
(defconst c++-feature-terminator-grpn 21)

(defconst c++-at-feature-regexp
  (concat c++-feature-decl-or-def
	  ;; This old version of the next line matched to things such as:
	  ;;   enum name {};.  Since such matches improperly end up in the
	  ;;   [function] default class, we only accept matches with () in
	  ;;   them.
	  ;;   "[ \t\n\r]*\\(\\[[^\{\;]+\\|([^:\{\;]*)"
	  "\\(\\(([^-+!:{}\;]*)\\|([^-+!(){}\;]*)\\)"
	  c++-routine-modifier-keyword "?\\)"
	  c++-comment-regexp)
  "Regexp matching a C++ method declaration or definition.
Member modifier keywords are grouped expression `c++-feature-mod-grpn'.
Member type is grouped expression `c++-feature-type-grpn', unless scoping
type name, grouped expression `c++-feature-scope-grpn' is non-nil, in which
case, grouping `c++-feature-scope-grpn' is the type plus \"::\". 
Member name is group `c++-feature-name-grpn'.  Function argument parentheses,
if any, are group `c++-feature-parens-grpn'.  All text following the argument
parentheses but preceding the opening brace is group
`c++-feature-post-args-grpn'.  Optional modifier keyword following the arguments
is group `c++-feature-post-modifier-grpn'.  Group
`c++-feature-defaults-grpn' optionally matches one of the three following
constructs: 
  1. base class constructor defaults (including the preceding colon);
  2. tail part of a pure virtual function (abstract method) declaration (= 0);
  3. the default value of an attribute (including the preceding equal sign).")

(defconst c++-feature-decl
  (concat c++-at-feature-regexp c++-feature-decl-terminator-regexp)
  "See documentation of `c++-at-feature-regexp' for grouping expressions.")

(defconst c++-friend-in-class
  (concat "^[ \t]*friend[ \t\n\r]+" c++-at-feature-regexp c++-feature-def-terminator-regexp)
  "See documentation of `c++-at-feature-regexp' for grouping expressions.
See `c++-friend-regexp' for that.")

(defconst c++-friend-class-regexp "^[ \t]*friend[ \t\n\r]+class[ \t\n\r]"
  "Regexp matching a C++ friend class declaration at the start of a line or the start of a string.")

(defconst c++-friend-regexp "^[ \t]*friend[ \t\n\r]"
  "Regexp matching a C++ friend declaration at the start of a line or the start of a string.")

(defconst c++-routine-def
  (concat "^" c++-at-feature-regexp c++-routine-def-terminator-regexp)
  "See documentation of `c++-at-feature-regexp' for grouping expressions.")

(defconst c++-routine-prefix-in-class
  (concat "^[ \t]*" c++-feature-decl-or-def
	  "\\(\\(([^:{}\;]*\\|([^(){}\;]*))\\)"
	  "\\([ \t\n\r]+const\\|[ \t\n\r]+mutable\\|[ \t\n\r]+restrict\\)?"
	  "[ \t\n\r]*"
	  "\\(\\(:[^\;\{]+\\)?" c++-comment-regexp))

(defconst c++-routine-in-class
  (concat c++-routine-prefix-in-class
	  "\\([\{\;]\\|=[ \t]*0[ \t]*\;\\)\\)\\)")
  "See documentation of `c++-at-feature-regexp' for grouping expressions.")

(defconst c++-routine-def-in-class
  (concat c++-routine-prefix-in-class
	  "\\(\{\\|=[ \t]*0[ \t]*\;\\)\\)\\)")
  "See documentation of `c++-at-feature-regexp' for grouping expressions.")

(defconst c++-feature-def
  (concat "^" c++-at-feature-regexp c++-feature-def-terminator-regexp)
  "See documentation of `c++-at-feature-regexp' for grouping expressions.")

(defconst c++-class-modifier-keyword
  "\\(\\(friend\\|public\\|protected\\)[ \t\n\r]+\\)")

(defconst c++-class-decl
  (concat c++-class-modifier-keyword "?"
	  c++-template-prefix
	  c++-class-keyword c++-identifier "[ \t]*[\;,]")
  "Regexp matching a C++ class declaration.
Template match, if any, is grouping `c++-decl-template-grpn'.
Class name is grouping `c++-class-name-grpn'.")

(defconst c++-decl-template-grpn 3)
(defconst c++-class-name-grpn 5)

(defconst c++-arg-identifier
  (concat "[_a-zA-Z][" c++-identifier-chars "]*")
  "Regular expression matching a C++ or G++ function argument identifier.")

(defconst c++-pure-virtual-function-regexp "\)[^=]*=[ \t]*0[ \t]*\;\\'"
  "Regexp matching the trailing part of a C++ pure virtual function signature.")

(defconst c++-c-type-htable
  (hash-make '(("enum"    . "[enumeration]")
	       ("struct"  . "[structure]")
	       ("typedef" . "[type]")
	       ("union"   . "[union]"))
	     t)
  "Hash table of complex C type keywords and associated default class names.")

(defconst c-identifier-chars "_a-zA-Z0-9"
  "String of chars and char ranges that may be used within a standard C++ template identifier.
This excludes the template arguments.")

(defconst c-identifier
  (concat "[\[_a-zA-Z]" "[\]" c-identifier-chars "]*"))

(defconst c-type-identifier
  (concat "[\[_a-zA-Z]" "[\]" c-identifier-chars "]*[ \t\n\r]*[*&]*"))

(defconst c-type-identifier-group
  ;; It is critical that the final part of this expression, [*& \t\n\r]+,
  ;; stay exactly as it is or certain feature definitions may be missed or
  ;; segmented improperly.
  ;; If you remove the `*&', "Int &operator=(int j) {}", will not be found
  ;; because the & will be missed.  If you change the `+' to a `*', "main()"
  ;; will show up as "- n" in listing buffers.
  (concat "\\(\\(" c-type-identifier "\\)[*& \t\n\r]+\\)"))

(defconst c-feature-decl-or-def
  (concat "\\(\\(" c++-type-modifier-keyword "*\\)"
	  c-type-identifier-group "\\)?"
	  "\\(" c-type-identifier "[ \t\n\r]*\\)?"
	  "[*&]?" c-identifier "[ \t\n\r]*")
  "Regexp matching the first part of a C feature declaration or definition.")

(defconst c-at-function-regexp
  (concat c-feature-decl-or-def
	  "\\(\\(([^-+!:{}\;]*)\\|([^-+!(){}\;]*)\\)"
	  c++-routine-modifier-keyword "?\\)"
	  c++-comment-regexp)
  "Regexp matching a C function declaration or definition.")

(defconst c-function-def-terminator-regexp
  "\\(\{\\)")

(defconst c-function-def
  (concat "^" c-at-function-regexp c-function-def-terminator-regexp))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun c++-add-default-classes ()
  (if br-c-tags-flag (c-add-default-classes)))

(defun c++-member-p ()
  "Prints whether entity at point is a C++ member definition or declaration."
  (interactive)
  (let ((name))
    (save-excursion
      (message
       (concat
	"Is " (if (c++-feature-def-p)
		  (progn (setq name
			       (br-buffer-substring (match-beginning
						     c++-feature-name-grpn)
						    (match-end
						     c++-feature-name-grpn)))
			 "")
		"not ")
	"a def.  "
	"Is " (if (and (c++-skip-to-statement) (c++-feature-decl))
		  (progn (setq name
			       (br-buffer-substring (match-beginning
						     c++-feature-name-grpn)
						    (match-end
						     c++-feature-name-grpn)))
			 "")
		"not ")
	"a member decl.  "
	(if name (concat "  Name = " name)))))))

(defun c++-feature-class-name ()
  "Return the class name for the function defined at point or nil if none.
If point is not within the brace delimited body of the function or class
declaration of a member function, then nil is returned."
  (let* ((opoint (point)))
    (save-excursion
      (if (or
	   ;;
	   ;; Case 1: Within a method whose definition includes its class name
	   ;;         which is also not within a class declaration.
	   ;;
	   (re-search-backward c++-routine-def nil t)
	   ;;
	   ;; Case 2: Within a method whose definition does include a class
	   ;;         name and which also is defined within a class declaration.
	   ;; Case 3: Within a function whose definition does not include a class
	   ;;         name and which is also not within a class declaration.
	   ;; Case 4: Within a method whose definition does not include a class
	   ;;         name but which is defined within a class declaration.
	   ;;
	   (re-search-backward c++-routine-def-in-class nil t))
	  (let ((class (if (match-beginning c++-feature-scope-grpn)
			   (buffer-substring
			    (match-beginning c++-feature-scope-grpn)
			    (- (match-end c++-feature-scope-grpn) 2)))))
	    (if class
		;; Case 2
		(progn (goto-char (1- (match-end 0)))
		       (if (eq (following-char) ?\{)
			   (condition-case ()
			       (progn (forward-list)
				      (if (> (point) opoint)
					  (c++-normalize-template-arguments
					   (br-delete-space class))))
			     (error nil))))
	      ;; Case 3 or 4
	      (c++-normalize-template-arguments
	       (c++-get-class-name-from-source))))))))

(defun c++-feature-implementors (name)
  "Return unsorted list of C++ feature tags which implement feature NAME.
This includes classes which define the interface for NAME as a pure virtual
function."
  (c++-feature-matches (concat "^" (regexp-quote name) "$")))

(defun c++-feature-edit-declaration (class-and-feature)
  "Edit in the other window (or the OO-Browser viewer window) the declaration source for CLASS-AND-FEATURE or for the listing entry at point.

If point is within the signature of a feature definition within a source code
buffer and a nil value for CLASS-AND-FEATURE is given, the feature's
declaration is displayed for editing.

With a prefix argument or outside of an OO-Browser listing buffer, prompt
with completion for CLASS-AND-FEATURE.  Point is left in the viewer window.
Signal an error if the declaration is not found."
  (interactive (list (if (or current-prefix-arg (not (br-browser-buffer-p)))
			 (let ((default (br-feature-default))
			       (class-and-feature
				(br-feature-complete 'must-match
						     "Edit feature declaration:")))
			   (if (and (equal default class-and-feature)
				    (save-excursion (c++-feature-def-p)))
			       ;; feature definitions are handled specially
			       nil
			     class-and-feature)))))
  (c++-feature-view-declaration class-and-feature t))

(defun c++-feature-view-declaration (&optional class-and-feature edit-flag)
  "View in the other window (or OO-Browser viewer window) the declaration source for optional CLASS-AND-FEATURE or for the listing entry at point.

If point is within the signature of a feature definition within a source code
buffer and a nil value for CLASS-AND-FEATURE is given, the feature's
declaration is displayed for viewing..

With a prefix argument or outside of an OO-Browser listing buffer, prompt
with completion for CLASS-AND-FEATURE.

Point is left in the original window unless the optional second argument
EDIT-FLAG is non-nil.  Signal an error if the declaration is not found."
  (interactive (list (if (or current-prefix-arg (not (br-browser-buffer-p)))
			 (let ((default (br-feature-default))
			       (class-and-feature
				(br-feature-complete 'must-match
						     "View feature declaration:")))
			   (if (and (equal default class-and-feature)
				    (save-excursion (c++-feature-def-p)))
			       ;; feature definitions are handled specially
			       nil
			     class-and-feature)))
		     nil))
  ;;
  ;; Pseudo-code (may be out dated):
  ;;   Move to class def of given feature or feature at point.
  ;;   Use the sig associated with feature listing to compute
  ;;     a regexp that matches to its declaration or definition
  ;;     within the class.
  ;;   Search forward for a match.
  ;;   If no match, then go to the first entry with the same name
  ;;     as feature.
  ;;
  (let ((oo-browser-listing (and (br-browser-buffer-p) (br-listing-window-p)))
	ftr-class)

    (cond (class-and-feature)
	  ;; If on an OO-Browser feature listing entry or within C++ source
	  ;; code on a feature definition signature, then leave
	  ;; class-and-feature as nil for now.
	  ((or (and oo-browser-listing (br-at-feature-p))
	       (save-excursion (c++-feature-def-p))))
	  ;; Otherwise, look for a default value around point.
	  (t (setq class-and-feature (br-feature-default))))

    (setq ftr-class (and (stringp class-and-feature)
			 (not (string-match "::" class-and-feature))
			 class-and-feature))
    (cond

     ((or ftr-class
	  (and (not class-and-feature)
	       oo-browser-listing
	       (not (br-at-feature-p))))
      ;; Assume is a class entry to display.
      (or (br-view nil edit-flag ftr-class)
	  (error "(OO-Browser):  `%s' declaration not found"
		 ftr-class)))

     ;; Show definitions of [default class] features rather than giving an
     ;; error since a user may invoke this command on them thinking he can
     ;; jump to them.
     ((and oo-browser-listing
	   (stringp class-and-feature) (eq (aref class-and-feature 0) ?\[))
      (if (if edit-flag
	      (br-edit-entry)
	    (br-view-entry))
	  ;; Above calls will either signal an error or return t or nil
	  ;; depending on whether the entry definition was found.
	  (message "(OO-Browser):  found definition of `%s'"
		   class-and-feature)))

     (t (let ((owind (selected-window))
	      (oframe (selected-frame))
	      (wconfig (current-window-configuration))
	      (err)
	      (ftr-tag)
	      (ftr-name)
	      (viewer-obuf)
	      (case-fold-search) ;; case-sensitive matching
	      class-end match ftr-sig)

	  (if (and (null class-and-feature)
		   (cond
		    (oo-browser-listing
		     (setq ftr-tag (br-feature-get-tag)
			   ftr-sig (and ftr-tag (br-feature-tag-signature ftr-tag)))
		     t)
		    ((save-excursion (c++-feature-def-p))
		     ;; Within a feature definition signature
		     (setq ftr-sig (buffer-substring
				    (match-beginning 0) (match-end 0)))
		     t)))
	      (setq class-and-feature (br-feature-signature-to-name ftr-sig t)
		    match (string-match "::" class-and-feature)
		    ftr-class (if match (substring
					 class-and-feature 0 match))
		    ftr-name (if match (substring class-and-feature (match-end 0))
			       class-and-feature))
	    ;; else
	    (if (stringp class-and-feature)
		(setq match (string-match "::" class-and-feature)
		      ftr-class (if match (substring class-and-feature 0 match)
				  "[function]")
		      ftr-name (if match (substring class-and-feature (match-end 0))
				 class-and-feature)
		      ftr-tag (car (br-feature-tag-and-file class-and-feature))))
	    (setq ftr-tag (or ftr-tag (br-feature-get-tag))
		  ftr-class (or ftr-class (and ftr-tag
					       (br-feature-tag-class ftr-tag)))
		  ftr-name (or ftr-name (and ftr-tag
					     (br-feature-tag-name ftr-tag)))
		  ftr-sig (and ftr-tag (br-feature-tag-signature ftr-tag))
		  class-and-feature (or class-and-feature
					(and ftr-class ftr-name
					     (concat ftr-class "::" ftr-name)))))

	  (unwind-protect
	      (cond ((and (not ftr-sig) ftr-class ftr-name)
		     (cond ((not (br-class-defined-p ftr-class))
			    (setq err
				  (format "(OO-Browser):  %s's class `%s' not defined within the Environment"
					  ftr-name ftr-class)))
			   ;; Try as a method and then as an attribute since
			   ;; it is not in the tags table.
			   ((or (c++-feature-method-display ftr-class ftr-name
							    ftr-name nil)
				(c++-feature-attribute-display ftr-class ftr-name
							       ftr-name nil))
			    (message
			     "(OO-Browser):  found declaration of `%s'"
			     class-and-feature))
			   (t (setq err (format
					 "(OO-Browser):  `%s' declaration not found"
					 class-and-feature)))))

		    ((not (and ftr-sig ftr-class ftr-name))
		     (setq err (format
				"(OO-Browser):  `%s' not declared within the Environment"
				class-and-feature)))

		    (t (if (br-in-browser) (br-to-view-window))
		       (setq viewer-obuf (current-buffer))
		       (if (and (br-find-class ftr-class nil)
				(search-forward "\{" nil t)
				(save-excursion
				  (backward-char)
				  ;; Move to class close brace but ignore
				  ;; any error if braces are unbalanced.
				  ;; Let the compiler tell the user about
				  ;; this.
				  (condition-case ()
				      (progn (forward-sexp)
					     (setq class-end (point)))
				    (error nil))))
			   (let ((case-fold-search)) ;; case-sensitive matching
			     (br-major-mode)
			     (let ((ftr-regexp
				    ;; Include args.
				    (c++-feature-declaration-regexp
				     ftr-sig t)))
			       (if (re-search-forward ftr-regexp class-end t)
				   (progn (br-display-code
					   (match-beginning 0))
					  (message
					   "(OO-Browser):  found declaration of `%s'"
					   class-and-feature))
				 ;; Try a simpler match without args
				 ;; as a fall back.
				 (setq ftr-regexp
				       (c++-feature-declaration-regexp
					ftr-sig nil))
				 (if (re-search-forward ftr-regexp class-end t)
				     (progn (br-display-code
					     (match-beginning 0))
					    (message "(OO-Browser):  Matched declaration by name only; failed to match arguments"))
				   (setq err
					 (format "(OO-Browser):  `%s' declaration not found"
						 class-and-feature)))))
			     (if err
				 nil
			       (setq buffer-read-only
				     (not (and edit-flag
					       (file-writable-p
						buffer-file-name))))))
			 (setq err (format
				    "(OO-Browser):  %s's class `%s' not defined within the Environment"
				    ftr-name ftr-class)))))
	    ;; unwindforms
	    (if err
		(progn
		  (if (not edit-flag) (select-window owind))
		  (if (eq (selected-frame) oframe)
		      (set-window-configuration wconfig))
		  (error err))
	      ;;
	      ;; Kill previous viewer buffer if unmodified and
	      ;; `br-keep-viewed-classes' is nil.
	      (if (and (buffer-live-p viewer-obuf)
		       (not (eq (current-buffer) viewer-obuf)))
		  (save-excursion
		    (set-buffer viewer-obuf)
		    (if (and (not br-keep-viewed-classes) buffer-read-only
			     (null (buffer-modified-p)))
			(kill-buffer viewer-obuf))))
	      (if (not edit-flag) (select-window owind)))))))))

(defun c++-feature-locate-p (feature-tag &optional regexp-flag)
  "Leave point at the start of FEATURE-TAG's definition in the current buffer.
Assume caller has moved point to the beginning of the buffer or to the point
of desired search start.
Optional REGEXP-FLAG means FEATURE-TAG is a regular expression."
  ;; Match to function definitions, not declarations, except for pure virtual
  ;; functions and friends which are declared, not defined, and so end with a
  ;; `;'.
  (let ((found t) (start) (case-fold-search)
	feature-sig feature-regexp class)
    (if (br-feature-tag-p feature-tag)
	(setq feature-sig (br-feature-tag-signature feature-tag)
	      class (br-feature-tag-class feature-tag))
      (setq feature-sig feature-tag
	    class nil))
    (if regexp-flag
	(if (stringp feature-tag)
	    (setq feature-regexp feature-tag)
	  (error "(c++-feature-locate-p): Not a regexp, %s" feature-tag)))
    ;;
    ;; First move to the proper class implementation if feature-sig does not
    ;; include a <class>:: part and this is not a [default-class], so that if
    ;; two classes in the same file have the same feature signature, we still
    ;; end up at the right one.
    (cond (class
	   (if (or (string-match "\\`\\[" class)
		   (and feature-sig (string-match "::" feature-sig)))
	       nil
	     (setq found (re-search-forward
			  (c++-class-definition-regexp class nil) nil t))))
	  ((string-match c++-tag-fields-regexp feature-sig)
	   (setq class (substring feature-sig
				  (match-beginning 1) (match-end 1))
		 feature-sig (substring feature-sig (match-end 0)))
	   (if (or (and regexp-flag
			(not (string-match "\\`\\\\\\[\\|::" feature-regexp)))
		   (not (or regexp-flag
			    (string-match "\\`\\[\\|::" feature-tag))))
	       (setq found
		     (re-search-forward
		      (c++-class-definition-regexp class regexp-flag)
		      nil t)))))
    ;;
    ;; If class was searched for and not found, return nil.
    (if (not found)
	nil
      ;; Otherwise, look for feature expression.
      (setq found nil)
      (or regexp-flag (setq feature-regexp
			    (c++-feature-signature-to-regexp feature-sig nil)))
      (while (and (or (re-search-forward feature-regexp nil t)
		      (and (not regexp-flag)
			   ;; Allow for comments between parameters.
			   (re-search-forward
			    (c++-feature-signature-to-regexp feature-sig t)
			    nil t)))
		  (setq start (match-beginning 0))
		  (not (setq found (not 
				    (if (c-within-comment-p)
					(progn (search-forward "*/" nil t)
					       t)))))))
      (if found
	  (progn
	    (message "(OO-Browser):  found definition of `%s'"
		     (c++-feature-signature-to-name feature-sig t nil))
	    (br-display-code start))))))

(defun c++-feature-name-to-regexp (name)
  "Converts feature NAME into a regular expression matching the feature's name tag."
  (concat "\\<" (c++-feature-signature-to-regexp name) "\\>"))

(defun c++-feature-args-regexp (func-args)
  (let* ((space "%%%")
	 (comment "!!!")
	 (obuf (current-buffer))
	 (tmp-buf (get-buffer-create c++-tmp-buffer-name)))
    (or tmp-buf (error "(OO-Browser):  (c++-feature-args-regexp) - Can't create tmp-buf"))
    (set-buffer tmp-buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    ;; Fill tmp-buffer with all func-args, including parens.
    (insert (br-regexp-quote func-args))

    (goto-char (point-min))
    (if (looking-at "(\\s-*)")
	(replace-match "(\\s-*)" t t)

      ;; Replace all "\( +" with "\(" temporarily
      (br-buffer-replace "\\(^\\|[^\\]\\)\([ \t\n\r]+" "\\1\(")
    
      ;; Replace all "+ \)" with "\)" temporarily
      (br-buffer-replace "[ \t\n\r]+\)" "\)")
    
      ;; Replace all "...\)" with "@@@" temporarily
      (br-buffer-replace "\\\\\\.\\\\\\.\\\\\\.\)" "@@@")

      ;; If an arg consists of 2 or more words and does not end with [*&>],
      ;; replace the last word with <identifier>.
      ;; Do this in two steps to avoid paren conflict with future replaces.
      (br-buffer-replace
       "\\([\(,][^=,\)]*[^ \t\n\r=,\)]+[ \t\n\r\\*&]+\\)[^ \t\n\r*&<>=,\)]+\\([ \t\n\r]*[,=\)]\\)"
       "\\1###\\2")

      ;; If an arg consists of only 1 word and does not end with [*&>], add an
      ;; <identifier> following it.
      ;; Do this in two steps to avoid paren conflict with future replaces.
      (br-buffer-replace
       "\\([\(,][ \t\n\r]*\\)\\([^ \t\n\r*&<>=,\)]+\\)\\([ \t\n\r]*[,=\)]\\)"
       "\\1\\2 ###\\3")

      ;; If an arg ends with [*&>], add an <identifier> following it.
      ;; Do this in two steps to avoid paren conflict with future replaces.
      (br-buffer-replace
       "\\([\(,][ \t\n\r]*\\)\\([^=,\)]+\\([\\]*[*&>]+\\)+\\)\\([ \t\n\r]*[,=\)]\\)"
       "\\1\\2 ###\\4")

      ;; Optionalize right hand side of argument defaults.
      ;; Do this after literal variable names have been changed to regexps or
      ;; variable names preceding the parentheses that this inserts will not
      ;; be changed to regexps.
      (br-buffer-replace "\\([^=,\( \t\n\r]+\\)\\([ \t\n\r]*=[^,\)]+\\)"
			 "\\1\\\\(\\2\\\\)?")

      ;; Replace all  " *, *" with "<comment>,<comment>"
      (br-buffer-replace "[ \t\n\r]*,[ \t\n\r]*" (concat comment "," comment))
    
      ;; Replace all " +" with "<spc>"
      (br-buffer-replace "[ \t\n\r]+" space)

      ;; Replace all "\(" with "\(<comment>"
      (br-buffer-replace "\\(^\\|[^\\]\\)\(" (concat "\\1\(" comment))
    
      ;; Replace all "\)" with "<comment>\)"
      (br-buffer-replace "\\([^\\]\\)\)" (concat "\\1" comment "\)"))

      ;; Replace all & and quoted \\* with "<spc>[*&]+<spc>"
      (br-buffer-replace "\\(\\(&\\|\\\\\\*\\)+\\)" (concat space "\\1" space))

      ;; Replace all "<spc>" with "[ \t\n\r]*"
      (br-buffer-replace space "[ \t\n\r]*" t)

      ;; Replace all "<comment>" with a comment regexp
      (br-buffer-replace comment c++-comment-regexp t)

      ;; Replace all "@@@" with any # of args
      (br-buffer-replace "@@@" "[^\)]*\)" t)

      (br-buffer-replace "###" (concat "\\\\(" c++-arg-identifier
				       "\\\\)?\\\\([ \t\n\r]*=[^,\)]+\\\\)?"))
      )

    ;; Return final buffer as a string.
    (prog1 (buffer-substring (point-min) (point-max))
      (set-buffer-modified-p nil)
      (set-buffer obuf))))

(defun c++-feature-signature-to-name (feature-sig-or-tag &optional with-class for-display)
  "Extracts the feature name from FEATURE-SIG-OR-TAG.
The feature's class name is dropped from FEATURE-SIG-OR-TAG unless optional
WITH-CLASS is non-nil.  If optional FOR-DISPLAY is non-nil, a feature type
character is prepended to the name for display in a browser listing."
  (if (br-feature-tag-p feature-sig-or-tag)
      (br-feature-tag-name feature-sig-or-tag with-class for-display)
    (let ((name))
      (cond
       ;; member
       ((string-match c++-tag-fields-regexp feature-sig-or-tag)
	(setq name (substring feature-sig-or-tag
			      (match-beginning (if for-display 2 3))
			      (match-end 3)))
	(if with-class
	    (setq name (concat
			(substring feature-sig-or-tag
				   (match-beginning 1) (match-end 1))
			"::" name)))
	;; Remove any trailing whitespace.
	(br-delete-space name))

       ((or (string-match c++-at-feature-regexp feature-sig-or-tag)
	    (string-match c++-attribute-tag-regexp feature-sig-or-tag))
	(setq name (substring feature-sig-or-tag
			      (match-beginning c++-feature-name-grpn)
			      (match-end c++-feature-name-grpn)))
	(if with-class
	    (if (match-beginning c++-feature-scope-grpn)
		(setq name (concat
			    (substring feature-sig-or-tag
				       (match-beginning c++-feature-scope-grpn)
				       (match-end c++-feature-scope-grpn))
			    name))
	      (if (br-class-in-table-p name)
		  ;; Constructor
		  (setq name (concat name "::" name)))))
	(if for-display (c++-feature-add-prefix
			 name "" feature-sig-or-tag) name))
       ;;
       ;; unknown
       (t;; Remove any trailing whitespace and add display prefix.
	(setq name (br-delete-space feature-sig-or-tag))
	(if (and with-class (br-class-in-table-p name))
	    ;; Constructor
	    (setq name (concat name "::" name)))
	(if for-display (c++-feature-add-prefix
			 name "" feature-sig-or-tag) name))))))

(defun c++-feature-signature-to-regexp (signature &optional comments-flag)
  "Given a C++ SIGNATURE, return regexp used to match to its definition."
  (setq signature (br-regexp-quote signature))
  (let ((prefix-info) (class))
    (if (string-match c++-tag-fields-regexp signature)
	(setq prefix-info (substring signature (match-beginning 0) (match-end 0))
	      class (substring signature (match-beginning 1) (match-end 1))
	      signature (substring signature (match-end 0))))
    ;; Add () to [function] tags whose signatures need them.
    (if (and (equal class "[function]")
	     (not (string-match "\(" signature)))
	(setq signature (concat signature " ( )")))
    (if (string-match "[^<>*&  \t]+\\(\<[^\>]+\>\\)[ \t\n\r]*::" signature)
	;; Method from a template class.  Match to a method with the same
	;; number of template parameters, regardless of parameter names.
	(let ((pre (substring signature 0 (match-beginning 1)))
	      (mid (substring signature (match-beginning 1) (match-end 1)))
	      (post (substring signature (match-end 1))))
	  (setq signature (concat pre (c++-template-args-regexp mid) post))))
    (if comments-flag
	(concat prefix-info
		(c++-feature-signature-commented-regexp signature))
      (concat prefix-info (c++-feature-signature-whitespace-regexp
			   signature)))))

(defun c++-feature-signature-commented-regexp (signature-regexp)
  (let* ((space "%%%")
	 (comment "!!!")
	 (obuf (current-buffer))
	 (tmp-buf (get-buffer-create c++-tmp-buffer-name)))
    (or tmp-buf (error "(OO-Browser):  (c++-feature-args-regexp) - Can't create tmp-buf"))
    (set-buffer tmp-buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    ;; Fill tmp-buffer with all of signature-regexp.
    (insert signature-regexp)

    (goto-char (point-min))

    ;; Replace all  " *, *" with "<comment>,<comment>"
    ;; Avoid commas separating template parameters which have already
    ;; been substituted for with "[^,<>]+".
    (br-buffer-replace "\\([^^,]\\)[ \t\n\r]*,[ \t\n\r]*"
		       (concat "\\1" comment "," comment))
    
    ;; Replace all "\(" with "<comment>\(<comment>"
    (br-buffer-replace "\\(^\\|[^\\]\\)\(" (concat "\\1" comment "\(" comment))
    
    ;; Replace all "\)" with "<comment>\)<comment>"
    (br-buffer-replace "\\([^\\]\\)\)" (concat "\\1" comment "\)" comment))

    ;; Replace all " +" with "<spc>"
    (br-buffer-replace "[ \t\n\r]+" space)

    ;; Replace any "<comment><comment>" with one "<comment>"
    (br-buffer-replace (concat comment comment) comment t)

    ;; Replace all "<comment>" with a comment regexp
    (br-buffer-replace (concat "\\(" space "\\)?" comment
			       "\\(" space "\\)?")
		       c++-comment-regexp t)

    ;; Replace all "<spc>" with "[ \t\n\r]*"
    (br-buffer-replace space "[ \t\n\r]*" t)

    ;; Return final buffer as a string.
    (prog1 (buffer-substring (point-min) (point-max))
      (set-buffer-modified-p nil)
      (set-buffer obuf))))

(defun c++-feature-signature-whitespace-regexp (signature-regexp)
  (let* ((obuf (current-buffer))
	 (tmp-buf (get-buffer-create c++-tmp-buffer-name)))
    (or tmp-buf (error "(OO-Browser):  (c++-feature-args-regexp) - Can't create tmp-buf"))
    (set-buffer tmp-buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    ;; Fill tmp-buffer with all of signature-regexp.
    (insert signature-regexp)
    (goto-char (point-min))
    (br-buffer-replace "[ \t\n\r]+" "[ \t\n\r]*" t)
    ;; Return final buffer as a string.
    (prog1 (buffer-substring (point-min) (point-max))
      (set-buffer-modified-p nil)
      (set-buffer obuf))))

(defun c++-feature-to-end ()
  ;; Move point to precede feature opening brace or declaration semicolon.
  (backward-char) 
  (if (eq (following-char) ?\{)
      (condition-case ()
	  ;; Move to end of feature but ignore any error if braces are
	  ;; unbalanced.  Let the compiler tell the user about this.
	  (forward-sexp)
	(error nil))))

(defun c++-scan-features ()
  "Return reverse ordered list of C++ routine definitions in current buffer.
Assume point is at the beginning of widened buffer and that all comments have
been removed by the caller.
This processes only those routines defined outside of the declaration of a
class; see `c++-scan-features-in-class' for the code that processes routines
and attributes defined within class declarations. It doesn't process global
variables since those are handled by separate code that deals with C constructs."
  (save-excursion
    (c-remove-functions) ;; Prevent any mis-scans within C function bodies.
    (let ((routines) class type name rout)
      (while (re-search-forward c++-routine-def nil t)
	(setq class ""
	      name (br-buffer-substring
		    (match-beginning c++-feature-name-grpn)
		    (match-end c++-feature-name-grpn))
	      type (if (match-beginning c++-feature-type-grpn)
		       (br-buffer-substring
			(match-beginning c++-feature-type-grpn)
			(match-end c++-feature-type-grpn))
		     ""))
	;;
	;; This code is very subtle but it does the right thing so don't
	;; change it unless you know exactly what you are doing.
	(if (not (match-beginning c++-feature-scope-grpn))
	    ;; This is a non-class function since we did not find a ::
	    ;; scoping operator.
	    ;; (setq class "[function]")
	    ;; Ignore global functions since these are found by a call to the
	    ;; `ootags' program.
	    (c++-feature-to-end)
	  (setq rout (br-buffer-substring (match-beginning 0) (match-end 0)))
	  ;;
	  ;; Is a member function, but this may set class = "" since prior
	  ;; regexp grouping may have grabbed the type.  Be careful to
	  ;; handle this.
	  (setq class (br-delete-space
		       (br-buffer-substring
			(match-beginning c++-feature-scope-grpn)
			(- (match-end c++-feature-scope-grpn) 2))))
	  (if (string-equal class "")
	      (setq class type))

	  (c++-feature-to-end)
	  ;; Ignore matches to class constructs and ::function references.
	  (if (or (string-equal class "")
		  (string-match c++-class-name-before rout))
	      nil
	    (setq rout (c++-feature-normalize rout class name)
		  routines (cons rout routines)))))
      routines)))

(defun c++-to-definition (&optional other-win)
  "Jumps to the definition of a selected C++ construct.
With OTHER-WIN non-nil, show it in another window.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If key is pressed:
 (1) on a `#include' statement, the include file is displayed;
     Look for include file in directory lists `smart-c-cpp-include-dirs'
     and `smart-c-include-dirs'.
 (2) within a method definition before the opening brace, its declaration is
     displayed; 
 (3) within a method declaration, its definition is displayed;
 (4) on a class name, the class definition is shown;
 (5) on a member reference (past any :: scoping operator), the member
     definition or a listing of possible definitions or a matching
     declaration (if no definitions exist within the Environment) is shown;
 (6) on a global variable or function identifier, its definition is shown.

 (2-5) require that an OO-Browser Environment has been loaded with
       the {M-x br-env-load RET} command."
  (interactive)
  (let ((obuf (current-buffer))
	(opoint (point)))
    (cond
     ((c++-include-file other-win))
     ((c++-feature other-win))
     ((and (goto-char opoint)
	   ;; If point is after a :: scoping operator or in front of a method
	   ;; call opening paren, then don't treat it as a class or search
	   ;; backward for the class name preceding ::.
	   (save-excursion
	     (skip-chars-forward "^\(\):\;,.?{}")
	     (not (looking-at "\(")))
 	   (br-check-for-class (c++-find-class-name) other-win)))
     ((br-check-for-class (save-excursion (c++-class-decl-p)) other-win))
     ;; Look it up as a member reference.
     ((and (goto-char opoint) (c++-feature-reference-definition)))
     ;; Look it up as a regular tag to find global variable and function definitions.
     ((progn
	(set-buffer obuf)
	(goto-char opoint)
	(smart-c++-tag))))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun c++-class-decl-p ()
  "Return nil unless point is within a class declaration, referenced by another
class.  Commented declarations also return nil.  When value is non-nil, it is
the class name from the declaration.  Leave point at start of statement for
visual clarity."
  (c++-skip-to-statement)
  (save-excursion
    (let ((class))
      (and (looking-at c++-class-decl)
	   (setq class (buffer-substring
			(match-beginning c++-class-name-grpn)
			(match-end c++-class-name-grpn)))
	   (if (match-beginning c++-decl-template-grpn)
	       (setq class
		     (c++-get-class-name
		      class (buffer-substring
			     (match-beginning c++-decl-template-grpn)
			     (match-end c++-decl-template-grpn))
		      t))
	     t)
	   (not (c-within-comment-p))
	   (progn (beginning-of-line)
		  (not (looking-at "[ \t]*//")))
	   class))))

(defun c++-feature (&optional other-win)
  "Move point to the definition/declaration of a method given by the declaration/definition at point.
This will immediately return nil if point is not within the signature, prior
to any arguments and any brace-delimited body."
  (interactive)
  (let ((feature-def) (ftr) (class) (ftr-pat)
	(opoint (point)))
    (c++-skip-to-statement)
    (if (and (= (point) (save-excursion (back-to-indentation) (point)))
	     (not (c-within-comment-p))
	     (save-excursion (beginning-of-line)
			     (not (looking-at "[ \t]*//")))
	     (not (looking-at c++-class-decl))
	     (looking-at (concat c++-at-feature-regexp "[\{\;,]")))
	(cond ((c++-feature-def-p)
	       ;; Within a feature definition
	       (if (c++-get-class-name-from-source)
		   ;; Definition is within a class, so it serves as its own
		   ;; declaration.
		   (progn
		     (message "(OO-Browser):  Method declaration/definition is one and the same.")
		     (recenter 0)
		     t)
		 (progn (c++-feature-edit-declaration nil)
			t)))
	      ((c++-view-friend other-win t))
	      ;; Now look for feature definition in code (non-header) files.
	      ((progn (setq feature-def (c++-feature-def-pat)
			    ftr (car (cdr (cdr feature-def)))
			    class (car (cdr feature-def))
			    ftr-pat (car feature-def))
		      (c++-locate-feature ftr class ftr-pat other-win)))
	      ((progn (goto-char opoint)
		      ;; Only match to declarations if within a class
		      ;; declaration.  Otherwise, unqualified method calls will
		      ;; improperly match as declarations, assuming non-ANSI C
		      ;; code may be used, e.g. method ();. 
		      (if (c++-get-class-name-from-source)
			  (save-excursion
			    (and (c++-skip-to-statement)
				 (c++-feature-decl)))))
	       (beep)
	       (message "(OO-Browser):  `%s' feature definition not found" ftr)
	       t))
      (goto-char opoint)
      nil)))

(defun c++-feature-add-prefix (feature-name class signature &optional friend-flag)
  "Add a browser listing display prefix to FEATURE-NAME from CLASS based on feature's SIGNATURE."
  (concat (cond
	   ;; friend declaration
	   (friend-flag "% ")
	   ;; pure virtual function (abstract method)
	   ((string-match c++-pure-virtual-function-regexp signature)
	    "> ")
	   ;; attribute
	   ((or (memq (aref signature (1- (length signature))) '(?\; ?=))
		(and (not (string-match "\(" signature))
		     ;; Don't match to complex C types like enumerations.
		     (not (br-default-class-p class))))
	    (if (string-match "\\<static\\>" signature)
		"& " ;; attribute shared by all instances
	      ;; regular attribute
	      "= "))
	   ;; constructor or destructor
	   ((or (eq ?~ (aref feature-name 0))
		(equal feature-name (c++-class-non-template-name class))
		(br-member feature-name
			   '("operator new" "operator delete")))
	    "+ ")
	   ;;
	   ;; Type declaration: enum, struct, union or typedef
	   ((and (string-match "\\`\\(struct\\|enum\\|union\\|typedef\\) " signature)
		 (not (string-match "\(" signature)))
	    "= ")
	   ;;
	   ;; Don't use the next clause since method declarations may contain
	   ;; a `static' when the definition does not and method declarations
	   ;; are ignored.
	   ;; ((string-match "\\<static\\>" signature) "1 ")
	   ;;
	   ;; methods
	   (t "- "))
	  feature-name))

(defun c++-feature-at-reference-p ()
  "Return C++ member reference name that point is within, else nil.
The return value is a list of:
\(variable member-reference-prefix class-name scoping-operator member-name method-flag)
where member-reference-prefix if non-nil is either \".\" or \"->\",
scoping-operator if non-nil is \"::\" and class-name may also be nil.
Method-flag is non-nil if this is a method reference."
  (let* ((identifier-chars "_:~<>a-zA-Z0-9")
	 (member-ref-regexp
	  (concat "\\(\\([_a-zA-Z][" identifier-chars "]*\\)"
		  ;; Allow for array references
		  "\\(\\s-*\\[[\]\[" identifier-chars "]*\\]\\)?\\)?"
		  ;; Match to . or -> and after
		  "\\(\\(\\(\\.\\|->\\)[_~:\<a-zA-Z][" identifier-chars "]*\\)?"
		  "\\([ \t]*\\([^\]\) \t:\;.,?~{}]\\)[^\[\( \t:\;.,~^!|?{}]?[=*]?\\)?\\)"))
	 variable op-identifier identifier class-name method-flag)
    (save-excursion
      (skip-chars-backward identifier-chars)
      (skip-chars-backward "-.>")
      ;; Move back over any array [] indexes
      (while (not (zerop (skip-chars-backward "\]")))
	(skip-chars-backward identifier-chars)
	(skip-chars-backward " \["))
      ;; Skip back over variable name itself
      (skip-chars-backward identifier-chars)
      (if (looking-at member-ref-regexp)
	  ;; Don't flash button here since it was already flashed before
	  ;; this was called.
	  (progn (setq variable
		       (if (match-beginning 2)
			   (buffer-substring (match-beginning 2) (match-end 2)))
		       op-identifier
		       (buffer-substring (match-beginning 4) (match-end 4))
		       identifier (if (match-beginning 5)
				      (buffer-substring
				       (match-beginning 5) (match-end 5)))
		       method-flag (if (match-beginning 8)
				       (equal "\(" (buffer-substring
						    (match-beginning 8)
						    (match-end 8)))))
		 (if (and variable (not identifier))
		     ;; e.g. unqualified_method();
		     (setq identifier variable
			   variable nil))
		 ;; Have to allow for identifiers such as, `operator () (int, int)'
		 ;; yet not include the opening parenthesis in `min ()'.
		 (if (string-match "\\<operator\\>" op-identifier)
		     (setq identifier op-identifier))
		 (if (and identifier
			  (string-match
			   "\\`\\(\\.\\|->\\)?\\([_<>a-zA-Z0-9]+[ \t\n\r]*::\\|[ \t\n\r]*::\\)?\\(.+\\)\\'"
			   identifier))
		     (list
		      ;; name of variable to which reference refers,
		      ;; this is nil if a more complicated expression is the
		      ;; reference anchor
		      variable
		      ;; . or -> variable-based reference
		      (if (match-beginning 1)
			  (substring identifier (match-beginning 1) (match-end 1)))
		      ;; class name
		      (or (and (match-beginning 2)
			       (> (length (setq class-name
						(substring identifier
							   (match-beginning 2)
							   (- (match-end 2) 2))))
				  0)
			       (save-match-data
				 (c++-normalize-template-arguments class-name)))
			  (and variable
			       (save-match-data (c++-feature-variable-class
						 variable))))
		      ;; scoping operator, ::
		      (if (match-beginning 2) "::")
		      ;; member name
		      (if (match-beginning 3)
			  (substring identifier (match-beginning 3) (match-end 3)))
		      ;; t if this is a method and not an attribute reference
		      method-flag)))))))

(defun c++-feature-attribute-display (class-name member-name reference-name message-flag)
  "Display CLASS-NAME's MEMBER-NAME attribute derived from REFERENCE-NAME.
MESSAGE-FLAG non-nil means display messages explaining whether the attribute was
found or not.  Return t if found and nil otherwise."
  (if (br-edit nil class-name)
      (let ((case-fold-search)) ;; case-sensitive matching
	(message "")
	(if (save-excursion (c++-feature-to-variable-declaration member-name))
	    (progn (br-display-code (match-beginning 5))
		   (if message-flag
		       (message
			"(OO-Browser):  `%s' declaration found within class `%s'"
			member-name class-name))
		   t)
	  (if message-flag
	      (progn (message "(OO-Browser):  `%s' not declared within class `%s'"
			      member-name class-name)
		     (beep)))
	  nil))
    (if message-flag
	(progn (message
		"(OO-Browser):  %s's class `%s' not defined within the Environment"
		reference-name member-name class-name)
	       (beep)))
    nil))

(defun c++-feature-decl ()
  (and (not (looking-at
	     "\\(if\\|else if\\|else\\|for\\|while\\|switch\\)\\s-*\("))
       (not (looking-at c++-class-decl))
       (looking-at c++-feature-decl)))

(defun c++-feature-declaration-regexp (def-sig &optional args-flag)
  "Return a regular expression that matches to the declaration of DEF-SIG.
If DEF-SIG is not a valid C++ declaration or definition, return nil.
Optional ARGS-FLAG non-nil means include argument matches in the regexp to
handle overloading."
  (and (not (string-match "\\<\\(if\\|else if\\|else\\|for\\|while\\|switch\\)\\s-*\("
			  def-sig))
       (string-match c++-at-feature-regexp def-sig)
       ;; Don't regexp-quote member-name yet.
       (let* ((member-name (substring def-sig
				      ;; Work around regexp bug that can drop
				      ;; leading [*&] char when there is no
				      ;; class:: preceding member-name.
				      (if (and (match-end c++-feature-type-grpn)
					       (= (match-end c++-feature-type-grpn)
						  (1- (match-beginning
						       c++-feature-name-grpn))))
					  (match-end c++-feature-type-grpn)
					(match-beginning c++-feature-name-grpn))
				      (match-end c++-feature-name-grpn)))
	      (member-modifiers (if (match-end c++-feature-mod-grpn)
				    (regexp-quote
				     (substring
				      def-sig
				      (match-beginning c++-feature-mod-grpn)
				      (match-end c++-feature-mod-grpn)))))
	      ;; Don't regexp-quote member-type yet.
	      (member-type (and (match-end c++-feature-type-grpn)
				;; member-type must be followed by whitespace
				;; or end with a [*&] character or we don't
				;; use it
				(memq (aref def-sig
					    (1- (match-end
						 c++-feature-type-grpn)))
				      '(?\  ?\n ?\t ?\r ?* ?&))
				;; Handle possible regexp bugs
				(not (equal
				      (match-beginning c++-feature-type-grpn)
				      (match-beginning c++-feature-name-grpn)))
				(substring
				 def-sig
				 (match-beginning c++-feature-type-grpn)
				 (match-end c++-feature-type-grpn))))
	      (func-args (if (and args-flag (match-end c++-feature-parens-grpn))
			     (br-feature-delete-c-comments
			      (substring
			       def-sig
			       (match-beginning c++-feature-parens-grpn)
			       (match-end c++-feature-parens-grpn)))))
	      (friend)
	      )
	 ;; Eliminate any leading or trailing whitespace in some matches.
	 (and member-type (string-match "[ \t\n\r]+\\'" member-type)
	      (setq member-type (substring member-type 0
					   (match-beginning 0))))
	 (and member-name (string-match "\\`[ \t\n\r]+" member-name)
	      (setq member-name (substring member-name (match-end 0))))

	 ;; Move any [*&] preceding member-name to member-type.
	 (if (string-match "\\`[*&]+" member-name)
	     (setq member-type (concat member-type
				       (substring member-name 0
						  (match-end 0)))
		   member-name (substring member-name (match-end 0))))

	 (setq member-type (br-regexp-quote member-type))

	 ;; Allow for different whitespace between declaration and definition
	 ;; when * or & is part of name and/or type, e.g. "char* id" and "char
	 ;; *id".
	 (if (and (stringp member-type)
		  (string-match "[\\*&]+\\'" member-type))
	     (setq member-type
		   (concat (substring member-type 0 (match-beginning 0))
			   "[ \t\n\r]*" (substring member-type
						   (match-beginning 0)))))
	 ;; Allow for trailing whitespace.
	 (and (stringp member-type)
	      (not (string-equal member-type ""))
	      (setq member-type (concat member-type "[ \t\n\r]*")))

	 (concat
	  "^[ \t]*"
	  c++-template-prefix
	  "\\(\\(auto\\|inline\\|overload\\|static\\|virtual\\)[ \t\n\r]+\\)?"
	  (if member-modifiers
	      (let ((def-mods "") (mod))
		(while (string-match "\\([a-z]+\\)[ \t\n\r]+"
				     member-modifiers)
		  (setq mod (substring member-modifiers
				       (match-beginning 1)
				       (match-end 1))
			member-modifiers (substring member-modifiers
						    (match-end 0)))
		  (if (not friend)
		      (setq friend (string-equal mod "friend")))
		  (if (equal (string-match
			      c++-type-def-modifier mod) 0)
		      (setq def-mods (concat def-mods "\\(" mod
					     "[ \t\n\r]+\\)?"))))
		def-mods))
	  (if (and member-type (equal (string-match "virtual" member-type) 0))
	      nil member-type)
	  (br-regexp-quote member-name)
	  "[ \t\n\r]*"
	  (if func-args (c++-feature-args-regexp func-args))))))

(defun c++-feature-def-p ()
  "Return nil unless point is within the first line of the signature of a member definition or pure virtual function.
The definition must be the first thing on the line and must not be commented
and point must be prior to any arguments or nil is returned.  Leaves point at
the start of the definition for visual clarity if t is returned."
  (let ((opoint (point)))
    (c++-skip-to-statement)
    (if (and (= (point) (save-excursion (back-to-indentation) (point)))
	     (not (c-within-comment-p))
	     (save-excursion (beginning-of-line)
			     (not (looking-at "[ \t]*//")))
	     (not (looking-at c++-class-decl))
	     (not (looking-at
		   "\\(if\\|else if\\|else\\|for\\|while\\|switch\\)\\s-*\("))
	     (looking-at (concat c++-at-feature-regexp "[\{\;,]"))
	     (let ((end-punct))
	       (or (eq ?\{ (setq end-punct (char-after (1- (match-end 0)))))
		   ;; If ends with a `[;,]' then must not have func parens
		   ;; nor simply be a scoped name in order to be a def.
		   ;; If it begins with `virtual', ends with "= 0" and has
		   ;; parens, then is a deferred virtual function declaration.
		   (if (match-end c++-feature-parens-grpn)
		       (save-restriction
			 (narrow-to-region (match-beginning 0) (match-end 0))
			 (if (looking-at
			      "\\s *\\<virtual\\>[^\;{}]+=[ \t]*0[ \t]*[,\;]")
			     (progn (message "(OO-Browser):  Pure virtual function, definition deferred to descendants")
				    t)))
		     (or (null (match-end c++-feature-scope-grpn))
			 (not (equal (concat
				      (buffer-substring
				       (match-beginning c++-feature-scope-grpn)
				       (match-end c++-feature-name-grpn))
				      (char-to-string end-punct))
				     (buffer-substring (match-beginning 0)
						       (match-end 0)))))))))
	t
      (goto-char opoint)
      nil)))

(defun c++-feature-def-pat ()
  "Return (list <feature-def-pat> <feature-class> <feature-name>) associated with declaration at point.
A call of: (funcall <feature-def-pat> <feature-class>) will generate a regexp 
which will match to the signature component of the feature's OO-Browser tag entry."
  (and (c++-skip-to-statement)
       (c++-feature-decl)
       ;; Don't regexp-quote member-name yet.
       (let* ((member-name (buffer-substring
			    ;; Work around regexp bug that can drop
			    ;; leading [*&] char when there is no class::
			    ;; preceding member-name.
			    (if (and (match-end c++-feature-type-grpn)
				     (= (match-end c++-feature-type-grpn)
					(1- (match-beginning
					     c++-feature-name-grpn))))
				(match-end c++-feature-type-grpn)
			      (match-beginning c++-feature-name-grpn))
			    (match-end c++-feature-name-grpn)))
	      (member-modifiers (if (match-end c++-feature-mod-grpn)
				    (br-quote-match c++-feature-mod-grpn)))
	      (class (if (match-end c++-feature-scope-grpn)
			 (buffer-substring
			  (match-beginning c++-feature-scope-grpn)
			  (- (match-end c++-feature-scope-grpn) 2))))
	      (scoped-name (match-end c++-feature-scope-grpn))
	      ;; Don't regexp-quote member-type yet.
	      (member-type (and (match-end c++-feature-type-grpn)
				;; Handle possible regexp bug
				(not
				 (equal 
				  (match-beginning c++-feature-type-grpn)
				  (match-beginning c++-feature-name-grpn)))
				(buffer-substring
				 (match-beginning c++-feature-type-grpn)
				 (match-end c++-feature-type-grpn))))
	      (func-args (if (match-end c++-feature-parens-grpn)
			     (br-feature-delete-c-comments
			      (buffer-substring
			       (match-beginning c++-feature-parens-grpn)
			       (match-end c++-feature-parens-grpn)))))
	      (friend)
	      )

	 (if (equal class "") (setq class nil))

	 ;; Eliminate any leading or trailing whitespace in some matches.
	 (and member-type (string-match "[ \t]+$" member-type)
	      (setq member-type (substring member-type 0
					   (match-beginning 0))))
	 (and member-name (string-match "^[ \t]+" member-name)
	      (setq member-name (substring member-name (match-end 0))))

	 ;; Set class if need be.
	 (cond (scoped-name)
	       (friend
		(if member-type
		    (progn
		      (setq class 
			    (string-match c++-identifier member-type))
		      (if class (setq class (substring
					     member-type class
					     (match-end 0)))))))
	       ;; Class name is not part of declaration
	       ;; nor a `friend' declaration, so look
	       ;; for declaration within a class
	       ;; definition and locate the class name.
	       ;; If not within a class, assume
	       ;; declaration is global.
	       (t (setq class (c++-get-class-name-from-source))))

	 ;; Move any [*&] preceding member-name to member-type.
	 (if (string-match "^[*&]+" member-name)
	     (setq member-type (concat member-type
				       (substring member-name 0
						  (match-end 0)))
		   member-name (substring member-name (match-end 0))))

	 (setq member-type (br-regexp-quote member-type))

	 ;; Allow for different whitespace between declaration and definition
	 ;; when * or & is part of name and/or type, e.g. "char* id" and "char
	 ;; *id".
	 (if (and (stringp member-type)
		  (string-match "[ \t\n\r]*\\([\\*&]+\\)\\'" member-type))
	     (setq member-type
		   (concat (substring member-type 0 (match-beginning 0))
			   "[ \t\n\r]*" (substring member-type
						   (match-beginning 1)))))
	 ;; Allow for trailing whitespace.
	 (and (stringp member-type)
	      (not (string-equal member-type ""))
	      (setq member-type (concat member-type "[ \t\n\r]*")))

	 (let ((pre-member-regexp
		(concat
		 c++-template-prefix
		 "\\(\\(auto\\|inline\\|overload\\|static\\|virtual\\)[ \t\n\r]+\\)?"
		 (if member-modifiers
		     (let ((def-mods "") (mod))
		       (while (string-match "\\([a-z]+\\)[ \t\n\r]+"
					    member-modifiers)
			 (setq mod (substring member-modifiers
					      (match-beginning 1)
					      (match-end 1))
			       member-modifiers (substring member-modifiers
							   (match-end 0)))
			 (if (not friend)
			     (setq friend (string-equal mod "friend")))
			 (if (equal (string-match
				     c++-type-def-modifier mod) 0)
			     (setq def-mods (concat def-mods "\\(" mod
						    "[ \t\n\r]+\\)?"))))
		       def-mods))
		 (if (and (stringp member-type)
			  (equal (string-match "virtual" member-type) 0))
		     nil member-type)))
	       (post-member-regexp
		(concat
		 ;; Point at beginning of line may imply a non-member func.
		 (if (or scoped-name (not (bolp))) "[ \t\n\r]*::[ \t\n\r]*")
		 (br-regexp-quote member-name)
		 "[ \t\n\r]*"
		 (if func-args
		     (concat "\\(" (c++-func-args-regexp func-args)
			     "\\|" (c++-func-args-string func-args)
			     "\\)"))
		 ;; If is a constructor member function, then can have some
		 ;; default values for base class constructors after a `:'
		 ;; but preceding the `{'.
		 "[ \t\n\r]*"
		 (and func-args
		      (equal member-name class)
		      "\\(:[^;{}]*\\)?")
		 c++-comment-regexp)))
	   (list
	    (` (lambda (class)
		 (concat "\\`" (, pre-member-regexp)
			 (if (not (eq (aref class 0) ?\[))
			     (br-regexp-quote class))
			 (, post-member-regexp))))
	    class member-name)))))

(defun c++-feature-map-tags (function regexp)
  "Apply FUNCTION to all current feature tags that match REGEXP and return a list of the results."
  (let ((identifier-chars (concat "[" c++-identifier-chars "]*"))
	(case-fold-search))
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

(defun c++-feature-matches (regexp)
  "Return an unsorted list of feature tags whose names match in part or whole to REGEXP.
^ and $ characters may be used to match to the beginning and end of a feature name,
respectively."
  (c++-feature-map-tags 'identity regexp))

(defun c++-feature-method-declaration (class-and-method-name)
  "Return a regexp matching the declaration of CLASS-AND-METHOD-NAME.
\(The class:: part is optional).  Grouping 0 starts the class name if given.
Grouping 1 matches the method name within the declaration."
  (let (class method)
    (if (string-match "::" class-and-method-name)
	(setq class (substring class-and-method-name 0 (match-beginning 0))
	      method (substring class-and-method-name (match-end 0)))
      (setq class nil
	    method class-and-method-name))
    (if class
	(format "\\<%s\\s-*::\\s-*\\(\\<%s\\>\\)\\s-*\("
		(regexp-quote class) (regexp-quote method))
      (format "\\(\\<%s\\>\\)\\s-*\(" (regexp-quote method)))))

(defun c++-feature-method-display (class-name method-name reference-name message-flag)
  "Display CLASS-NAME's METHOD-NAME method derived from REFERENCE-NAME.
MESSAGE-FLAG non-nil means display messages explaining whether the attribute was
found or not.  Return t if found and nil otherwise."
  (if (br-edit nil class-name)
      (let ((case-fold-search)) ;; case-sensitive matching
	(message "")
	(if (save-excursion
	      (re-search-forward
	       (c++-feature-method-declaration method-name)
	       nil t))
	    (progn (br-display-code (match-beginning 0))
		   (if message-flag
		       (message
			"(OO-Browser):  `%s' found within class `%s'"
			method-name class-name))
		   t)
	  (if message-flag
	      (progn (message "(OO-Browser):  `%s' not declared within class `%s'"
			      method-name class-name)
		     (beep)))
	  nil))
    (if message-flag
	(progn (message
		"(OO-Browser):  %s's class `%s' not defined within the Environment"
		reference-name method-name class-name)
	       (beep)))
    nil))

(defun c++-feature-normalize (feature class name &optional friend-flag)
  "Return a feature tag based on FEATURE, CLASS, NAME and optional FRIEND-FLAG."
  (setq class (br-delete-space class)
	name (c++-feature-add-prefix name class feature friend-flag))
  (concat class c++-type-tag-separator
	  name c++-type-tag-separator 
	  (br-feature-delete-c-comments feature)))

(defun c++-feature-reference-definition ()
  "Jumps to the definition of or lists possible definitions of a C++ member reference.
If no definitions are found and there is a . or -> reference prefix, then it
attempts to display the declaration for the reference.
Returns t if a member (non-global) definition or declaration is found,
otherwise returns nil."
  (let* ((member-elts (c++-feature-at-reference-p))
         (variable (nth 0 member-elts))
         (member-reference-prefix (nth 1 member-elts))
         (class-name (nth 2 member-elts))
         (scoping-operator (nth 3 member-elts))
         (member-name (nth 4 member-elts))
         (method-flag (nth 5 member-elts))
	 (found)
	 (err)
	 result)
    (setq result
	  (if (and member-elts (fboundp 'br-feature-display-implementors))
	      (condition-case ()
		  (progn
		    (message "Looking for `%s'..." member-name)
		    (cond
		     ;;
		     ;;      1. ::global_function(...) or ::variable
		     ;;        * Ignore, global functions and variables are
		     ;;          handled by another function.
		     ((and (null class-name) scoping-operator)
		      (setq found t)
		      (message "")
		      nil)
		     ;;
		     ;;      2. class::scoped_method(...) or class::scoped_attribute
		     ((and class-name scoping-operator)
		      ;;       * Look up scoped_member in class.
		      ;;       * Look up scoped_member in each
		      ;;	 ancestor until find a match.
		      ;;
		      ;;         Refine this procedure in the future
		      ;;         by accepting only those methods with the same
		      ;; 	 number of arguments as the call.
		      ;;
		      ;;         Also refine by looking for declarations through
		      ;;         all ancestors of class if no definitions are
		      ;;         found.
		      ;;
		      ;;         Also refine to handle class::construct->member()
		      ;;         situations where the class is known.
		      ;;
		      (setq found
			    (if method-flag
				(if (zerop (br-feature-ancestor-implementors
					    class-name member-name method-flag))
				    (progn
				      (setq err
					    (format
					     "(OO-Browser):  `%s::%s' definition not found"
					     class-name member-name))
				      nil)
				  t)
			      ;; attribute - goto class def and then search for it
			      (c++-feature-attribute-display
			       class-name member-name member-name t)))
		      t)
		     ;;
		     ;;      3. variable.method(...) or variable.attribute
		     ;;         variable-ptr->method(...) or variable-ptr->attribute
		     (member-reference-prefix
		      ;;
		      ;;       * Determine the static class of the variable.
		      ;;         If it is declared within the current file:
		      ;;           follow the steps in #2 except list all
		      ;;           ancestral matches rather than just the
		      ;;           first to account for dynamic binding.
		      ;;         Else if no variable type can be determined,
		      ;;           list all known implementors of the method or
		      ;;           display the definition if there is only one 
		      ;;           match.
		      ;;
		      (setq found
			    (if (and variable
				     (or class-name
					 (setq class-name
					       (c++-feature-variable-class
						variable))))
				(if method-flag
				    (if (zerop (br-feature-ancestor-implementors
						class-name member-name method-flag))
					(progn
					  (setq err
						(format
						 "(OO-Browser):  `%s::%s' definition not found"
						 class-name member-name))
					  nil)
				      t)
				  ;; attribute - goto class def and then
				  ;;             search for it
				  (c++-feature-attribute-display
				   class-name member-name
				   (concat variable member-reference-prefix
					   member-name) t))
			      (if method-flag
				  (if (br-feature-display-implementors member-name)
				      (progn (message "") t)))))
		      t)
		     ;;
		     ;;      4. method(...)
		     ;;         ~destructor(...)
		     ((and (null class-name) (null scoping-operator))
		      ;;
		      ;;       * Get the class of the method/function
		      ;;         in which the call is made.  If this
		      ;;         enclosing context is not a global function,
		      ;;         follow the steps in #3.
		      ;;       * If not found yet, ignore it and it will be
		      ;;         looked up as a global function by another
		      ;;         function.
		      ;;
		      (if (setq class-name (c++-feature-class-name))
			  (setq found (not (zerop (br-feature-ancestor-implementors
						   class-name member-name
						   method-flag))))
			(setq found t)
			(message "")
			nil))
		     (t (setq err "(OO-Browser):  (c++-feature-reference-definition) - unrecognized context")
			nil)))
		(error nil))))

      (if (and (not found) member-reference-prefix class-name member-name)
	  ;; This will signal an error if no declaration is found; return t
	  ;; in case it does not.
	  (progn (c++-feature-view-declaration
		  (concat class-name "::" member-name) t)
		 t)
	(if err
	    (progn (message err) (beep)))
	result)))

(defun c++-feature-variable-class (variable)
  "Search backward from point looking for a type declaration of VARIABLE.
Return the class of the variable which may differ from its type (due to * and & operators)."
  ;; Pseudo-variable `this' refers to the current class.
  (if (string-equal variable "this")
      (c++-feature-class-name)
    (save-excursion
      (let ((class))
	(while (and (c++-feature-to-item-declaration variable)
		    (setq class (buffer-substring
				 (match-beginning 3) (match-end 3)))
		    (br-member class '("delete" "new")))
	  (setq class nil))
	(if class (c++-normalize-template-arguments class))))))

(defvar c++-feature-item-regexp1
   "\\(^\\|[\;\(]\\)\\s-*\\(\\(%s\\)[*& \t\n\r]+\\)\\([_a-zA-Z][^\;,\(\)]*,\\)*\\s-*\\(\\<%s\\>\\)\\s-*"
   "Grouping 2 matches the item's type, possibly with trailing whitespace.
Grouping 3 matches its class (dropping any * or & characters).  Grouping 5
matches the item name within the declaration.")

(defvar c++-feature-item-regexp2
   "\\(,\\)\\s-*\\(\\(%s\\)[*& \t\n\r]+\\)\\(\\)\\s-*\\(\\<%s\\>\\)\\s-*"
   "Grouping 2 matches the item's type, possibly with trailing whitespace.
Grouping 3 matches its class (dropping any * or & characters).  Grouping 5
matches the item name within the declaration.")

(defun c++-feature-to-item-declaration (item)
  "Search backwards to a match for the declaration of ITEM name and return point or nil if no match.
See the documentation string for `c++-feature-item-regexp' for a description
of matched groupings."
  (or (re-search-backward
       (format (concat c++-feature-item-regexp1 "[\[\(=\;,*&\)]")
	       c++-identifier (regexp-quote item)) nil t)
      (re-search-backward
       (format (concat c++-feature-item-regexp2 "[\[\(=\;,*&\)]")
	       c++-identifier (regexp-quote item)) nil t)))

(defun c++-feature-to-variable-declaration (variable)
  "Search backwards to a match for the declaration of VARIABLE name and return point or nil if no match.
See the documentation string for `c++-feature-item-regexp' for a description
of matched groupings."
  (or (re-search-backward
       (format (concat c++-feature-item-regexp1 "[\[=\;,*&\)]")
	       c++-identifier (regexp-quote variable)) nil t)
      (re-search-backward
       (format (concat c++-feature-item-regexp2 "[\[=\;,*&\)]")
	       c++-identifier (regexp-quote variable)) nil t)))

(defun c++-files-with-source (class)
  "Use CLASS to compute set of files that match to a C++ source file regexp.
Return as a list."
  (let ((file (if class (br-class-path class) buffer-file-name)))
    (and file
	 (let* ((src-file-regexp (concat "^" (br-filename-head file)
					 c++-code-file-regexp))
		(dir (file-name-directory file))
		(files (directory-files dir nil src-file-regexp)))
	   (mapcar (function (lambda (f) (expand-file-name f dir)))
		   files)))))

(defun c++-find-ancestors-feature (class-list ftr-pat &optional other-win)
  "Scan ancestors of CLASS-LIST and show feature definition matching FTR-PAT.
Optional OTHER-WIN means display the result in another window."
  ;; If no class, search for non-member function.
  (or class-list (setq class-list '(nil)))
  (br-feature-display class-list ftr-pat other-win))

(defun c++-find-class-name ()
  "Return current word as a potential class name."
  (save-excursion
    (let* ((start)
	   (ignore "-\]\[& \t\n\r\;,.\(\){}*")
	   (pat (concat "^" ignore)))
      (forward-char 1)
      (skip-chars-backward ignore)
      (skip-chars-backward pat)
      (setq start (point))
      (skip-chars-forward (concat pat ":"))
      (buffer-substring start (point)))))

(defun c++-func-args-regexp (func-args)
  (let* ((space "%%%")
	 (obuf (current-buffer))
	 (tmp-buf (get-buffer-create c++-tmp-buffer-name)))
    (or tmp-buf (error "(OO-Browser):  (c++-func-args-regexp) - Can't create tmp-buf"))
    (set-buffer tmp-buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    ;; Fill tmp-buffer with all func-args, including parens.
    (insert (br-regexp-quote func-args))

    (goto-char (point-min))
    (if (looking-at "(\\s-*)")
	(replace-match "(\\s-*)" t t)

      ;; Replace all "\( +" with "\(" temporarily
      (br-buffer-replace "\\(^\\|[^\\]\\)\([ \t\n\r]+" "\\1\(")
    
      ;; Replace all "+ \)" with "\)" temporarily
      (br-buffer-replace "[ \t\n\r]+\)" "\)")
    
      ;; Replace all "...\)" with "@@@" temporarily
      (br-buffer-replace "\\\\\\.\\\\\\.\\\\\\.\)" "@@@")

      ;; Optionalize right hand side of argument defaults.
      (br-buffer-replace "\\([^=,\( \t\n\r]+\\)\\([ \t\n\r]*=[^,\)]+\\)"
			 (concat "\\1\\\\( "
				 c++-arg-identifier
				 "\\\\)? \\\\(\\2\\\\)?"))

      ;; Replace all "\)" with "optional <c++-identifier> \)"
      (br-buffer-replace
       ;; Yes, all this complexity really is necessary.
       "\\([\\\(,][^=\)]*[^\\=\)]+\\)\\(\\\\*\)\\)"
       (concat "\\1\\\\( " c++-arg-identifier
	       "\\\\)?\\2"))

      ;; Replace all  "," with "optional <c++-identifier>,"
      (br-buffer-replace
       "\\([\(,][^=,]+\\),"
       (concat "\\1\\\\( " c++-arg-identifier "\\\\)?,"))

      ;; Replace all  " *, *" with "<space>,<space>"
      (br-buffer-replace "[ \t\n\r]*,[ \t\n\r]*" (concat space "," space))
    
      ;; Replace all " +" with "<spc>"
      (br-buffer-replace "[ \t\n\r]+" space)

      ;; Replace all "\(" with "\(<space>"
      (br-buffer-replace "\\(^\\|[^\\]\\)\(" (concat "\\1\(" space))
    
      ;; Replace all "\)" with "<space>\)"
      (br-buffer-replace "\\([^\\]\\)\)" (concat "\\1" space "\)"))

      ;; Replace all & and quoted \\* with "<spc>[*&]+<spc>"
      (br-buffer-replace "\\(\\(&\\|\\\\\\*\\)+\\)" (concat space "\\1" space))

      ;; Replace all "<spc>"
      (br-buffer-replace space " *" t)

      ;; Replace all "@@@" with any # of args
      (br-buffer-replace "@@@" "[^\)]*\)" t)
      )

    ;; Return final buffer as a string.
    (prog1 (buffer-substring (point-min) (point-max))
      (set-buffer-modified-p nil)
      (set-buffer obuf))))

(defun c++-func-args-string (func-args)
  (let* ((space "%%%")
	 (obuf (current-buffer))
	 (tmp-buf (get-buffer-create c++-tmp-buffer-name)))
    (or tmp-buf (error "(OO-Browser):  (c++-func-args-string) - Can't create tmp-buf"))
    (set-buffer tmp-buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    ;; Fill tmp-buffer with all func-args, including parens.
    (insert (br-regexp-quote func-args))

    (goto-char (point-min))
    (if (looking-at "(\\s-*)")
	(replace-match "(\\s-*)" t t)

      ;; Replace all "\( +" with "\(" temporarily
      (br-buffer-replace "\\(^\\|[^\\]\\)\([ \t\n\r]+" "\\1\(")
    
      ;; Replace all "+ \)" with "\)" temporarily
      (br-buffer-replace "[ \t\n\r]+\)" "\)")
    
      ;; Replace all "...\)" with "@@@" temporarily
      (br-buffer-replace "\\\\\\.\\\\\\.\\\\\\.\)" "@@@")

      ;; If an arg consists of 2 or more words and does not end with [*&>],
      ;; replace the last word with <identifier>.
      (br-buffer-replace
       "\\([\(,][^=,\)]*[^ \t\n\r=,\)]+[ \t\n\r\\*&]+\\)[^ \t\n\r*&<>=,\)]+\\([ \t\n\r]*[,=\)]\\)"
       (concat "\\1" c++-arg-identifier "\\2"))

      ;; If an arg consists of only 1 word and does not end with [*&>], add an
      ;; <identifier> following it.
      (br-buffer-replace
       "\\([\(,][ \t\n\r]*\\)\\([^ \t\n\r*&<>=,\)]+\\)\\([ \t\n\r]*[,=\)]\\)"
       (concat "\\1\\2 " c++-arg-identifier "\\3"))

      ;; If an arg ends with [*&>], add an <identifier> following it.
      (br-buffer-replace
       "\\([\(,][ \t\n\r]*\\)\\([^=,\)]+\\([\\]*[*&>]+\\)+\\)\\([ \t\n\r]*[,=\)]\\)"
       (concat "\\1\\2 " c++-arg-identifier "\\4"))

      ;; Optionalize right hand side of argument defaults.
      ;; Do this after literal variable names have been changed to regexps or
      ;; variable names preceding the parentheses that this inserts will not
      ;; be changed to regexps.
      (br-buffer-replace "\\([^=,\( \t\n\r]+\\)\\([ \t\n\r]*=[^,\)]+\\)"
			 "\\1\\\\(\\2\\\\)?")

      ;; Replace all  " *, *" with "<space>,<space>"
      (br-buffer-replace "[ \t\n\r]*,[ \t\n\r]*" (concat space "," space))
    
      ;; Replace all " +" with "<spc>"
      (br-buffer-replace "[ \t\n\r]+" space)

      ;; Replace all "\(" with "\(<space>"
      (br-buffer-replace "\\(^\\|[^\\]\\)\(" (concat "\\1\(" space))
    
      ;; Replace all "\)" with "<space>\)"
      (br-buffer-replace "\\([^\\]\\)\)" (concat "\\1" space "\)"))

      ;; Replace all & and quoted \\* with "<spc>[*&]+<spc>"
      (br-buffer-replace "\\(\\(&\\|\\\\\\*\\)+\\)" (concat space "\\1" space))

      ;; Replace all "<spc>"
      (br-buffer-replace space " *" t)

      ;; Replace all "@@@" with any # of args
      (br-buffer-replace "@@@" "[^\)]*\)" t)
      )

    ;; Return final buffer as a string.
    (prog1 (buffer-substring (point-min) (point-max))
      (set-buffer-modified-p nil)
      (set-buffer obuf))))

(defun c++-get-class-name-from-source ()
  "Return class name from the class definition that encloses point or nil."
  (let ((opoint (point))
	(class))
    (save-excursion
      (if (re-search-backward c++-class-def-regexp nil t)
	  (progn (goto-char (match-beginning c++-class-def-derived-grpn))
		 (setq class (c++-normalize-class-match nil))
		 ;; Ensure that declaration occurs within class definition.
		 (condition-case ()
		     (progn (forward-list)
			    (if (> (point) opoint)
				class))
		   (error
		    (error "(OO-Browser):  (c++-get-class-name-from-source) - Mismatched class definition braces"))))))))

(defun c++-output-feature-tags (feature-file feature-tags-list)
  "Write C++ FEATURE-FILE's FEATURE-TAGS-LIST into `br-feature-tags-file'.
Assume `br-feature-tags-init' has been called."
  (interactive)
  (save-excursion
    (br-feature-set-tags-buffer)
    (goto-char 1)
    ;; Delete any prior feature tags associated with feature-file.
    (let (start)
      ;; There may be more than one set of entries for each file name.
      (while (search-forward feature-file nil 'end)
	(forward-line -1)
	(setq start (point))
	(search-forward "\^L" nil 'end 2)
	(backward-char 1)
	(delete-region start (point))))
    (if feature-tags-list
	(progn (insert "\^L\n")
	       ;; Quote pathname to avoid read errors on MS OSes.
	       (prin1 feature-file (current-buffer))
	       (insert "\n")
	       (mapcar (function (lambda (tag) (insert tag "\n")))
		       feature-tags-list)))))

(defun c++-include-file (&optional other-win)
  "If point is on an include file line, try to display file.
Return non-nil iff an include file line, even if file is not found.
Look for include file in `c++-cpp-include-dirs' and in directory list
`c++-include-dirs'."
  (let ((opoint (point)))
    (beginning-of-line)
    (if (looking-at c++-include-regexp)
	(let ((incl-type (string-to-char
			  (buffer-substring (match-beginning 1)
					       (1+ (match-beginning 1)))))
	      (file (buffer-substring (match-beginning 2) (match-end 2)))
	      (path)
	      (dir-list c++-include-dirs)
	      (found))
	  (goto-char opoint)
	  (setq dir-list (if (eq incl-type ?\<)
			     (append dir-list c++-cpp-include-dirs)
			   (cons (file-name-directory buffer-file-name)
				 dir-list)))
	  (while dir-list
	    (setq path (concat (car dir-list) file)
		  dir-list (if (setq found (file-exists-p path))
			       nil
			     (cdr dir-list))))
	  ;;
	  ;; If not found in normal include dirs, check all Env paths also.
	  ;;
	  (if (not found)
	      (let ((paths (delq nil (hash-map 'cdr br-paths-htable))))
		(while paths
		  (setq path (car paths))
		  (if (string-equal (file-name-nondirectory path) file)
		      (setq found t paths nil)
		    (setq paths (cdr paths))))))
	  ;;
	  ;; If found, display file
	  ;;
	  (if found
	      (if (file-readable-p path)
		  (progn
		    (funcall br-edit-file-function path other-win)
		    (if (not (fboundp 'br-lang-mode))
			(c++-mode-setup))
		    (br-major-mode))
		(beep)
		(message "(OO-Browser):  Include file `%s' unreadable" path))
	    (beep)
	    (message "(OO-Browser):  Include file `%s' not found" file))
	  path)
      (goto-char opoint)
      nil)))

(defun c++-locate-feature (ftr class ftr-pat &optional other-win)
  ;; `class' may = nil, implying non-member function
  (or class (setq class "[function]"))
  (let ((def-class))
    (if (and ftr-pat
	     (setq def-class
		   (c++-find-ancestors-feature (list class)
					       ftr-pat other-win)))
	(progn (if (and class (not (equal class def-class)))
		   (message
		     "Member `%s' of class `%s' inherited from class `%s'."
		     ftr class def-class))
	       t))))

(defun c++-scan-ancestors-feature (class-list ftr-pat &optional other-win)
  "Display feature definition derived from CLASS-LIST, matching FTR-PAT.
Scan files with same base name as class file."
  (let  ((classes class-list)
	 (found-ftr)
	 (code-def-files)
	 (file)
	 (ftr-sig-regexp)
	 (class))
    (if (null class-list)
	nil
      (while (and (not found-ftr) classes)
	(setq class (car classes)
	      code-def-files (c++-files-with-source class)
	      ftr-sig-regexp (funcall ftr-pat class))
	(while (and (setq file (car code-def-files))
		    (not (setq found-ftr
			       (br-feature-found-p file ftr-sig-regexp
						   nil other-win t))))
	  (setq code-def-files (cdr code-def-files)))
	(setq classes (if found-ftr nil (cdr classes))))
      (if found-ftr
	  (or class t)
	(c++-scan-ancestors-feature
	 (apply 'append (mapcar (function (lambda (cl) (br-get-parents cl)))
				class-list))
	 ftr-pat)))))

(defun c++-scan-features-in-class (class start end)
  "Return reverse ordered list of C++ feature (attribute and method) definitions within CLASS declaration.
START and END give buffer region to search.
Assume that all comments have been removed by the caller.
This scans only those features defined within the declaration of a class;
see `c++-scan-features' for the code that processes features defined elsewhere."
  (setq class (br-delete-space class))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (let ((features) signature name type type-modifiers)
	;;
	;; Get friend routine declarations.
	;;
	(while (re-search-forward c++-friend-in-class nil t)
	  (setq start (match-beginning 0)
		name (br-buffer-substring
		      (match-beginning c++-feature-name-grpn)
		      (match-end c++-feature-name-grpn))
		type (if (match-beginning c++-feature-type-grpn)
			 (br-buffer-substring
			  (match-beginning c++-feature-type-grpn)
			  (match-end c++-feature-type-grpn)))
		signature (br-buffer-substring (match-beginning 0)
					       (match-end 0))
		;; Do this after done getting groupings from the search.
		type (if type (br-delete-space type)))
	  ;; Get rid of this friend so it doesn't slow down later scanning.
	  (delete-region start (point))
	  ;; Handle type conversion ops: operator int() { return i; }
	  (if (equal type "operator") (setq name (concat type " " name)
					    type nil))
	  (setq signature (c++-feature-normalize signature class name t)
		features (cons signature features)))
	;;
	;; Get method definitions and pure virtual declarations.
	;;
	(goto-char (point-min))
	(while (re-search-forward c++-routine-in-class nil t)
	  (setq start (match-beginning 0)
		name (br-buffer-substring
		      (match-beginning c++-feature-name-grpn)
		      (match-end c++-feature-name-grpn))
		type (if (match-beginning c++-feature-type-grpn)
			 (br-buffer-substring
			  (match-beginning c++-feature-type-grpn)
			  (match-end c++-feature-type-grpn)))
		signature (br-buffer-substring
			   (match-beginning 0) (match-end 0)))
	  (if (and (string-equal
		    "\;" (buffer-substring (match-beginning
					    c++-feature-terminator-grpn)
					   (1+ (match-beginning
						c++-feature-terminator-grpn))))
		   (not (string-match c++-pure-virtual-function-regexp signature)))
	      ;; Routine declaration only, ignore and delete
	      ;; this declaration so that it doesn't slow down later
	      ;; scanning.
	      (progn (forward-line 1)
		     (delete-region start (point)))
	    ;; Do this after all other needed groupings are extracted from the
	    ;; initial regexp match.
	    (setq type (if type (br-delete-space type)))
	    ;; Move point to precede signature opening brace or declaration
	    ;; semicolon.
	    (backward-char)
	    (if (eq (following-char) ?\{)
		(condition-case ()
		    ;; Move to end of feature but ignore any error if braces
		    ;; are unbalanced.  Let the compiler tell the user about
		    ;; this.
		    (progn (forward-sexp)
			   (forward-line 1)
			   ;; Get rid of this method so it doesn't slow down
			   ;; later scanning.
			   (delete-region start (point)))
		  (error nil)))
	    ;; Handle type conversion ops:  operator int() { return i; }
	    (if (equal type "operator") (setq name (concat type " " name)
					      type nil))
	    (setq signature (c++-feature-normalize signature class name)
		  features (cons signature features))))
	;;
	;; Get attributes and `friend class' declarations.
	;;
	(goto-char (point-min))
	(while (re-search-forward c++-attribute-decl nil t)
	  (setq start (match-beginning 0)
		name (br-buffer-substring
		      (match-beginning c++-feature-name-grpn)
		      (match-end c++-feature-name-grpn))
		type-modifiers
		(if (match-beginning c++-feature-mod-grpn)
		    (br-buffer-substring
		     (match-beginning c++-feature-mod-grpn)
		     (match-end c++-feature-mod-grpn)))
		type (if (match-beginning c++-feature-type-grpn)
			 (br-buffer-substring
			  (match-beginning c++-feature-type-grpn)
			  (match-end c++-feature-type-grpn)))
		signature (br-buffer-substring
			   (match-beginning 0)
			   (if (match-beginning c++-attribute-value-grpn)
			       ;; include the = preceding a value
			       (1+ (match-beginning c++-attribute-value-grpn))
			     (match-end 0)))
		;; Do this after done getting groupings from the search.
		type (if type (br-delete-space type)))
	  ;; Move point to precede declaration opening brace (enum, struct,
	  ;; union or typedef) or declaration semicolon.
	  (backward-char)
	  (if (eq (following-char) ?\{)
	      (condition-case ()
		  ;; Move to end of feature but ignore any error if braces
		  ;; are unbalanced.  Let the compiler tell the user about
		  ;; this.
		  (forward-sexp)
		(error nil)))
	  (if (or (and type
		       (string-match
			"\\(\\`\\|[ \t\n\r]\\)\\(enum\\|struct\\|union\\|typedef\\)\\(\\'\\|[ \t\n\r]\\)"
			type))
		  (if (and type-modifiers
		       (string-match
			"\\(\\`\\|[ \t\n\r]\\)\\(enum\\|struct\\|union\\|typedef\\)\\(\\'\\|[ \t\n\r]\\)"
			type-modifiers))
		      (if (equal (substring type-modifiers
					    (match-beginning 2) (match-end 2))
				 "typedef")
			  (progn (setq type "typedef")
				 t))))
	      ;; This is a type declaration which may be followed
	      ;; by one or more attribute names of the type.  Add
	      ;; a feature entry for each attribute.
	      (progn (while (and (not (eq (preceding-char) ?\;))
				 (looking-at
				  (concat "\\s-*" c++-identifier "\\s-*[,;]")))
		       (goto-char (match-end 0))
		       (if (equal type "typedef")
			   (setq features
				 (cons (c++-feature-normalize
					signature
					(hash-get type c++-c-type-htable)
					(concat class "::"
						(br-buffer-substring
						 (match-beginning 1)
						 (match-end 1)))
					nil)
				       features))
			 (setq features (cons (c++-feature-normalize
					       signature class
					       (br-buffer-substring
						(match-beginning 1)
						(match-end 1)) nil)
					      features))))
		     ;; If the previously found type declaration has its
		     ;; own name, then add a feature attribute for it.
		     (if (and type (hash-get type c++-c-type-htable))
			 (setq signature (c++-feature-normalize
					  signature
					  (hash-get type c++-c-type-htable)
					  (concat class "::" name)
					  nil)
			       features (cons signature features)))
		     (forward-line 1))
	    ;; Ignore false matches to operator= () and the like triggered by
	    ;; the =.
	    (or (string-match "\\<operator\\>[^=\n\r]*=" signature)
		(setq signature
		      (c++-feature-normalize
		       signature class name
		       (if (string-match
			    c++-friend-class-regexp signature) t))
		      features (cons signature features)))))
	features))))

(defun c++-skip-to-statement ()
  (if (re-search-backward "\\(^\\|[\;{}()]\\)[ \t]*" nil t)
      (progn (goto-char (match-end 0))
	     (skip-chars-forward " \t")
	     t)))

(defun c++-view-friend (&optional other-win sig-at-point-flag)
  "With point on a friend listing entry or friend signature (prior to any arguments) in a source code buffer, view its source code definition.
With optional OTHER-WIN non-nil, display in another window.
With optional SIG-AT-POINT-FLAG non-nil, assume point is within a
signature in a source buffer."
  (interactive)
  (let ((opoint (point))
	(tag
	 (if (not sig-at-point-flag)
	     (br-feature-get-tag)
	   (c++-skip-to-statement)
	   ;; Must be the first thing on the line.
	   (and (= (point) (save-excursion (back-to-indentation) (point)))
		(progn (beginning-of-line)
		       (looking-at c++-friend-regexp))
		(looking-at c++-friend-in-class)
		(let ((friend (buffer-substring (match-beginning 0)
						   (match-end 0)))
		      (class (c++-get-class-name-from-source)))
		  (c++-feature-normalize
		   friend class
		   (c++-feature-signature-to-name friend) t))))))
    (cond ((or (null tag)
	       ;; Not a friend entry.
	       (not (eq ?% (aref (c++-feature-signature-to-name tag nil t) 0))))
	   (goto-char opoint)
	   nil)
	  ((eq ?\{ (aref tag (1- (length tag))))
	   ;; Friend is defined where declared.
	   (br-feature 'view tag)
	   t)
	  ((string-match (format " class \\(%s\\) ?;$"
				 c++-return-type-identifier) tag)
	   ;; friend class
	   (br-view nil nil
		    (c++-normalize-template-arguments
		     (substring tag (match-beginning 1) (match-end 1))))
	   t)
	  (t
	   (if sig-at-point-flag
	       nil ;; Other feature location code will handle this.
	     (br-feature t tag) ;; Move point to friend declaration.
	     (c++-feature other-win))))))

(defun c-remove-functions ()
  "Delete any C function definitions from the buffer.
These are handled by the ootags program for the OO-Browser and leaving
them in could cause invalid scans within their bodies.  Assume point is at
the beginning of widened buffer and that all comments have been removed by
the caller.  This processes only those functions defined outside of class
declarations."
  (save-excursion
    (goto-char (point-min))
    (condition-case ()
	(while (re-search-forward c-function-def nil t)
	  (delete-region (match-end 0)
			 (progn (c++-feature-to-end) (point))))
      (error nil))))

(provide 'br-c++-ft)
