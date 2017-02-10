;;!emacs
;;
;; FILE:         br-objc-ft.el
;; SUMMARY:      Objective-C OO-Browser class and feature functions.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     c, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:    03-Oct-90
;; LAST-MOD:     10-May-01 at 03:03:40 by Bob Weiner
;;
;; Copyright (C) 1990-1997  BeOpen.com
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'br-objc)
(require 'objc-brows)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defconst objc-default-category-class "[category]"
  "Name of the default class whose instances are Objective-C categories.")

(defconst objc-default-protocol-class "[protocol]"
  "Name of the default class whose instances are Objective-C protocols.")

(defconst objc-type-identifier
  (concat "[a-zA-Z][" objc-identifier-chars "]*[ \t\n\r]*[*&]*"))

(defconst objc-type-tag-separator "@"
  "String that separates a tag's type from its normalized definition form.")

(defconst objc-tag-fields-regexp
  ;; The \\\\? below is necessary because we sometimes use this expression to
  ;; test against a string that has ben regexp-quoted and some of the
  ;; characters in br-feature-type-regexp will then be preceded by \\.
  (format "^\\([^%s*& \n]+\\)%s\\\\?\\(%s \\)?\\([^%s\n]+\\)\\(%s\\|\\'\\)"
	  objc-type-tag-separator objc-type-tag-separator
	  br-feature-type-regexp objc-type-tag-separator
	  objc-type-tag-separator)
 "Regexp matching the fields of an Objective-C feature tag line.
Group 1 is the class of the feature.  Optional group 2 is the prefix
preceding the feature when displayed within a listing buffer.  Group 3 is the
feature name.  The feature definition signature begins at the end of the
regexp match, i.e. (match-end 0), and goes to the end of the string or line.")

(defvar objc-cpp-include-dirs '("/usr/include/")
  "*Ordered list of include directories by default searched by C preprocessor.
Each directory must end with a directory separator.  See also
`objc-include-dirs'.")

(defvar objc-include-dirs nil
  "*Ordered list of directories to search for Objective-C include files.
Each directory must end with a directory separator.  Directories normally
searched by the Objective-C pre-processor should be set instead in
`objc-cpp-include-dirs'.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun objc-add-default-classes ()
  (br-add-default-classes
   (list objc-default-category-class objc-default-protocol-class))
  (if br-c-tags-flag (c-add-default-classes)))

(defun objc-class-definition-name (class)
  "Convert CLASS name to the way it appears in its source code definition.
Returns a regular expression."
  (cond ((string-match "^<.+>$" class)
	 ;; Remove <> delimiters from protocol class.
	 (regexp-quote (substring class 1 -1)))
	((string-match "^\\([^ \(]+\\) *(\\([^\)]*\\)) *$" class)
	 ;; Allow for whitespace within class(category)
	 (format "%s[ \t\n\r]*([ \t\n\r]*%s[ \t\n\r]*)"
		 (regexp-quote
		  (substring class (match-beginning 1) (match-end 1)))
		 (regexp-quote
		  (substring class (match-beginning 2) (match-end 2)))))
	((string-match "^(\\([^\)]*\\)) *\\([^ ]+\\) *$" class)
	 ;; Allow for whitespace within (category)class
	 (format "%s[ \t\n\r]*([ \t\n\r]*%s[ \t\n\r]*)"
		 (regexp-quote
		  (substring class (match-beginning 2) (match-end 2)))
		 (regexp-quote
		  (substring class (match-beginning 1) (match-end 1)))))
	(t (regexp-quote class))))

(defun objc-class-definition-regexp (class)
  "Return regexp to uniquely match the definition of CLASS name."
  (concat (if (eq (aref class 0) ?<)
	      objc-protocol-before
	    objc-interface-before)
	  (objc-class-definition-name class)
	  objc-class-name-after))

(defun objc-feature-implementors (name)
  "Return unsorted list of Objective-C feature tags which implement feature NAME."
  ;; Eliminate matches to protocol signatures (recognized by a leading `<' in
  ;; the class name field) since these are interfaces and not implementations.
  (delq nil
	(mapcar (function (lambda (sig) (if (eq (aref sig 0) ?<) nil sig)))
		(objc-feature-matches (concat "^" (regexp-quote name) "$")))))

(defun objc-feature-locate-p (feature-tag &optional regexp-flag)
  "Leaves point at the start of FEATURE-TAG's definition in the current buffer.
Assumes caller has moved point to the beginning of the buffer or to the point
of desired search start.
Optional REGEXP-FLAG means FEATURE-TAG is a regular expression."
  ;; Match to function definitions, not declarations.
  (let ((found) (start) (case-fold-search)
	feature-sig class)
    (if (br-feature-tag-p feature-tag)
	(setq feature-sig (br-feature-tag-signature feature-tag)
	      class (br-feature-tag-class feature-tag))
      (setq feature-sig feature-tag
	    class nil))
    ;; First move to the proper class implementation if this is not a
    ;; [default-class], so that if two classes in the same file have the same
    ;; feature signature, we still end up at the right one.
    (if (or class
	    (and (not (string-match (if regexp-flag "\\`\\\\\\[" "\\`\\[")
				    feature-sig))
		 (string-match objc-tag-fields-regexp feature-sig)
		 (setq class (substring feature-sig
					(match-beginning 1) (match-end 1)))))
	;; Protocols don't define methods, they only declare them, so we
	;; know we can't be searching for a protocol method definition
	;; here, and so there is no special case handling.
	(re-search-forward (concat objc-implementation-before
				   ;; Assume regexp-quoted class is the
				   ;; same as non-regexp-quoted version
				   ;; since this call will regexp-quote it
				   ;; again; we have no way of
				   ;; un-regexp-quoting it.
				   (objc-class-definition-name class))
			   nil t))
    ;;
    ;; Now search for feature.
    (or regexp-flag (setq feature-tag
			  (objc-feature-signature-to-regexp feature-sig)))
    (while (and (re-search-forward feature-tag nil t)
		(setq start (match-beginning 0))
		(not (setq found (not (if (c-within-comment-p)
					  (progn (search-forward "*/" nil t)
						 t)))))))
    (if found (br-display-code start))))

(defun objc-feature-name-to-regexp (name)
  "Converts feature NAME into a regular expression matching the feature's name tag."
  (cond
   ;;
   ;; protocol name
   ((eq (aref name 0) ?\<) (regexp-quote name))
   ;;
   ;; category name
   ((eq (aref name 0) ?\()
    ;; Drop any class name following the category.
    (regexp-quote
     (substring name 0 (1+ (string-match "\)" name)))))
   ;;
   ;; feature tag
   ((string-match objc-tag-fields-regexp name)
    (concat
     "^" (regexp-quote (substring name (match-beginning 0) (match-end 0)))))
   ;;
   ;; feature listing entry
   ((zerop (string-match br-feature-type-regexp name))
    (regexp-quote (concat objc-type-tag-separator name objc-type-tag-separator)))
   ;;
   ;; unrecognized name format
   (t (error "(objc-feature-name-to-regexp): Invalid name, `%s'" name))))

(defun objc-feature-signature-to-name (feature-sig-or-tag &optional with-class for-display)
  "Extracts the feature name from FEATURE-SIG-OR-TAG.
FEATURE-SIG-OR-TAG may be a feature tag or a signature extracted from source
code.  The feature's class name is dropped from FEATURE-SIG-OR-TAG unless
optional WITH-CLASS is non-nil.  The feature's type (class feature = +,
instance feature = -) is dropped unless FOR-DISPLAY is non-nil."

  ;; feature tag
  (cond
   ((br-feature-tag-p feature-sig-or-tag)
    (br-feature-tag-name feature-sig-or-tag with-class for-display))
    
   ((string-match objc-tag-fields-regexp feature-sig-or-tag)
    (cond ((and with-class for-display)
	   (substring feature-sig-or-tag (match-beginning 1) (match-end 3)))
	  (for-display
	   (substring feature-sig-or-tag
		      (match-beginning (if (match-beginning 2) 2 3))
		      (match-end 3)))
	  (with-class
	   (concat 
	    (substring feature-sig-or-tag (match-beginning 1) (1+ (match-end 1)))
	    (substring feature-sig-or-tag (match-beginning 3) (match-end 3))))
	  (t (substring feature-sig-or-tag (match-beginning 3) (match-end 3)))))

   ;; source code feature-sig-or-tag
   (t (let ((loop-p t)
	    (name-part (concat "\\`" objc-name-part objc-name-sep))
	    (name-type (concat "\\`" objc-name-part objc-type-sep))
	    (name))
	(cond ((or (eq (aref feature-sig-or-tag 0) ?\<)
		   (eq (aref feature-sig-or-tag 0) ?\())
	       ;; protocol or category tags
	       (setq name feature-sig-or-tag))
	      ((string-match (concat "\\`" br-feature-type-regexp)
			     feature-sig-or-tag)
	       ;; regular feature signature
	       (if for-display 
		   (setq name (concat (substring feature-sig-or-tag
						 (match-beginning 0)
						 (match-end 0))
				      " ")))
	       (setq feature-sig-or-tag
		     (concat (substring feature-sig-or-tag
					(match-end 0)) "\;"))
	       (while (and loop-p (string-match name-part feature-sig-or-tag))
		 ;; Handles named or unnamed parameters.
		 (if (match-beginning objc-name-part-id)
		     (setq name (concat name
					(substring
					 feature-sig-or-tag
					 (match-beginning objc-name-part-id)
					 (match-end objc-name-part-id)))))
		 (if (not (eq (aref feature-sig-or-tag (1- (match-end 0))) ?:))
		     (setq loop-p nil
			   feature-sig-or-tag
			   (substring feature-sig-or-tag (match-end 0)))
		   (setq name (concat name ":")
			 feature-sig-or-tag
			 (substring feature-sig-or-tag (match-end 0)))
		   (if (string-match name-type feature-sig-or-tag)
		       (setq feature-sig-or-tag
			     (substring feature-sig-or-tag (match-end 0)))))))
	      (t (error
		  "(objc-feature-signature-to-name): Invalid signature, `%s'"
		  feature-sig-or-tag)))
	name))))

(defun objc-feature-signature-to-regexp (signature)
  "Return regexp to match the definition of an Objective-C element SIGNATURE.
SIGNATURE may be a feature tag or a signature extracted from source code."
  (cond ((string-match
	  (format
	   "^\\([^%s*& \n]+\\)%s\\(%s \\)?[^%s\n]+%s\\([\<\(][^\>\)\n]+[\>\)]\\(%s\\)?\\)"
	   objc-type-tag-separator objc-type-tag-separator br-feature-type-regexp
	   objc-type-tag-separator objc-type-tag-separator objc-identifier)
	  signature)
	 ;; protocol or category signature
	 (let ((class (substring signature (match-beginning 1) (match-end 1)))
	       (element (substring signature (match-beginning 3)
				  (match-end 3))))
	   (if (eq (aref element 0) ?\<)
	       (if (string-equal class objc-default-protocol-class)
		   ;; find def of protocol
		   (concat objc-protocol-before
			   (objc-class-definition-name element)
			   "[\< \t\n\r]")
		 ;; find def of class which conforms to protocol
		 (concat objc-interface-before
			 (objc-class-definition-name class)
			 objc-class-name-after
			 "[^\>\{]*[\<,][ \t\n\r]*"
			 (objc-class-definition-name element)
			 "[ \t\n\r]*[\>,]"))
	     (if (string-equal class objc-default-category-class)
		 ;; find def of `[category]@(category-name)class-name'
		 (concat objc-interface-before
			 (objc-class-definition-name element))
	       ;; find def of `class-name@(category-name)'
	       (concat objc-interface-before
		       (objc-class-definition-name (concat class element)))))))
	;;
	(t
	 ;; regular feature tag
	 (setq signature (regexp-quote signature))
	 (if (string-match objc-tag-fields-regexp signature)
	     ;;
	     ;; We must leave the class name as an optional component at the
	     ;; start of the signature so that functions that lookup feature
	     ;; definitions can use it to ensure that the definition is found
	     ;; within the right class.
	     (setq signature
		   (concat
		    "\\(" (substring signature (match-beginning 1)
				     (match-end 1))
		    objc-type-tag-separator "\\)?"
		    (substring signature (match-end 0)))))
	 (let ((pat) (i 0) (c) (len (length signature)))
	   (while (< i len)
	     (setq c (aref signature i)
		   pat (cond ((eq c ? )
			      (concat pat "[ \t\n\r]*"))
			     (t
			      (concat pat (char-to-string c))))
		   i (1+ i)))
	   (if (eq ?{ (aref pat (1- (length pat))))
	       (setq pat (concat (substring pat 0 -1)
				 "\\([ \t\n\r]*//.*[\n]\\)*[ \t\n\r]*\{"))
	     pat)))))

(defun objc-list-categories (class)
  "Return sorted list of Objective-C CLASS categories."
  (cond ((string-equal class objc-default-category-class)
	 (br-list-features class))
	((eq (aref class 0) ?\[)
	 ;; Any other default classes belong to no categories.
	 nil)
	(t 
	 (br-feature-map-class-tags
	  (function (lambda (tag)
		      (if (string-match "\\`\(" (br-feature-tag-name tag))
			  tag)))
	  class))))

(defun objc-list-category-classes (category categories)
  "Given a CATEGORY name and the full list of class CATEGORIES in the Environment, return the list of classes that implement CATEGORY."
  (let (category-regexp)
    (cond ((not (eq categories t))
	   (if (string-match "\([^\)]+\)" category)
	       (setq category-regexp
		     (regexp-quote
		      (substring category (match-beginning 0)
				 (match-end 0))))
	     (error
	      "(OO-Browser):  (objc-list-category-classes) - Invalid category, %s"
	      category))
	   (let* ((case-fold-search)
		  (classes
		   (delq nil
			 (mapcar
			  (function
			   (lambda (class-category)
			     (if (string-match category-regexp
					       class-category)
				 (substring class-category
					    (match-end 0)))))
			  categories))))
	     (sort classes 'string-lessp)))
	  (t
	   (error "(OO-Browser):  (objc-list-category-classes) - No categories found in the Environment")))))

;;; The OO-Browser depends on the name of this next function; don't change it.
(defun objc-list-protocols (class)
  "Return a sorted list of the parent protocols of CLASS."
  (sort (delq nil (mapcar (function (lambda (parent)
				      (if (eq (aref parent 0) ?<)
					  parent)))
			  (br-get-parents class)))
	'string-lessp))

(defun objc-routine-at-point-p ()
  "Returns name of Objective-C routine signature at point or nil.
If called interactively, it prints the name in the minibuffer."
  (interactive)
  (save-excursion
    (if (and (re-search-backward "[-+\n\r]\\|\\`" nil t)
	     (looking-at "[ \t\n\r]*[-+]"))
	(let ((name "") (loop-p t)
	      (name-part (concat objc-name-part objc-name-sep))
	      (name-type (concat objc-name-part objc-type-sep)))
	  (goto-char (match-end 0))
	  (while (and loop-p (looking-at name-part))
	    ;; Handles named or unamed parameters.
	    (if (match-beginning objc-name-part-id)
		(setq name (concat name
				   (buffer-substring
				     (match-beginning objc-name-part-id)
				     (match-end objc-name-part-id)))))
	    (goto-char (match-end 0))
	    (if (not (eq (preceding-char) ?:))
		(setq loop-p nil)
	      (setq name (concat name ":"))
	      (if (looking-at name-type) (goto-char (match-end 0)))
	      ))
	  (if (interactive-p)
	      (message name)
	    name)))))

(defun objc-scan-features ()
  "Return reverse ordered list of Objective-C methods in the current buffer.
Assume point is at the beginning of a widened buffer."
  (save-excursion
    (let ((routines) (rout) (class-end)
	  class category)
      (while (re-search-forward
	      (concat "^@implementation[ \t\n\r]+" objc-identifier
		      "\\([ \t\n\r]*\([ \t\n\r]*" objc-identifier
		      "[ \t\n\r]*\)\\)?")
	      nil t)
	(setq category (if (match-beginning 3)
			   (br-buffer-substring (match-beginning 3)
						(match-end 3)))
	      class (br-buffer-substring (match-beginning 1) (match-end 1))
	      class (if category (format "%s(%s)" class category) class))
	(save-excursion
	  (if (search-forward "\n@end" nil t)
	      (setq class-end (point))
	    (error "(objc-scan-features): %s, at char %d, @implementation without @end"
		   class (point))))
	(while (re-search-forward objc-routine-def class-end t)
	  (setq rout (br-buffer-substring (match-beginning 0)
					  (match-end 0)))
	  (if (c-within-comment-p)
	      (search-forward "*/" nil t)
	    (backward-char) ;; Move point to precede feature opening brace.
	    (condition-case ()
		;; Move to end of feature but ignore any error if braces are
		;; unbalanced.  Let the compiler tell the user about this.
		(forward-sexp)
	      (error nil))
	    (setq rout (objc-feature-normalize rout class)
		  routines (cons rout routines)))))
      routines)))

(defun objc-scan-protocol-list ()
  "Return a list of <protocol names> following point, delimited by <> and separated by commas.
Point may be immediately before or after the `<' which begins the protocol
list.  Leaves point afer the closing delimiter of the protocol list."
  (cond ((eq (preceding-char) ?\<))
	((eq (following-char) ?\<)
	 (forward-char 1))
	(t
	 (error "(objc-scan-protocol-list): Point must precede or follow a `<' delimiter.")))
      (let ((end (save-excursion (search-forward "\>")))
	    (protocols))
	(while (re-search-forward objc-identifier end t)
	  (setq protocols (cons (concat "\<"
					(br-buffer-substring (match-beginning 1)
							     (match-end 1))
					"\>")
				protocols)))
	(goto-char end)
	(nreverse protocols)))

(defun objc-scan-protocol-signatures ()
  "Return reverse ordered list of Objective-C protocol method signatures in the current buffer.
Assume point is after the name of the protocol but before any method signature."
  (let ((signatures) (sig) (protocol-end)
	protocol)
    (save-excursion
      (if (or (not (re-search-backward (concat "^@protocol[ \t\n\r]+" objc-identifier
					       "\\|^@end")
				       nil t))
	      (not (match-beginning 1)))
	  (error "(objc-scan-protocol-signatures): Called when point was not within a protocol definition.")
	(setq protocol (concat "\<" (br-buffer-substring (match-beginning 1)
							 (match-end 1)) "\>"))
	(save-excursion
	  (if (search-forward "\n@end" nil t)
	      (setq protocol-end (point))
	    (error "(objc-scan-protocol-signatures): %s, at char %d, @protocol without @end.")))
	(while (re-search-forward objc-feature-declaration protocol-end t)
	  (setq sig (br-buffer-substring (match-beginning 0) (match-end 0)))
	  (if (c-within-comment-p)
	      (search-forward "*/" nil t)
	    (setq sig (objc-feature-normalize sig protocol)
		  signatures (cons sig signatures))))
	signatures))))

(defun objc-to-definition (&optional other-win)
  "If point is within a declaration or global reference, try to move to its definition.
With OTHER-WIN non-nil, show it in another window."
  (interactive)
  (let ((opoint (point)))
    (cond
     ((objc-include-file other-win))
     ((objc-feature other-win))
     ((and (goto-char opoint)
	   (br-check-for-class (objc-find-class-name) other-win)))
     ((br-check-for-class (objc-class-decl-p) other-win))
     ;; Look it up as a regular tag to find global identifier definitions.
     ((and (goto-char opoint) (smart-objc-tag))))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun objc-class-decl-p ()
  "Return nil unless point is within a class declaration, referenced by another
class.  Commented declarations also return nil.  When value is non-nil, it is
the class name from the declaration.  Leave point at start of statement for
visual clarity."
  (objc-skip-to-statement)
  (save-excursion
    (let ((class))
      (and (looking-at objc-class-decl)
	   (setq class (buffer-substring
			(match-beginning objc-class-name-grpn)
			(match-end objc-class-name-grpn)))
	   (not (c-within-comment-p))
	   (progn (beginning-of-line)
		  (not (looking-at "[ \t]*//")))
	   class))))

(defun objc-feature (&optional other-win)
  "Move point to the definition of the element given by the declaration at point.
Point must be within the declaration signature and not within its
brace-delimited body or this will simply return nil.
Return nil if point is anywhere other than within an element signature."
  ;; If `\{' follows the element declaration, then the element is defined right
  ;; here, within the class definition.
  (interactive)
  (let ((opoint (point)))
    (objc-skip-to-statement)
    (if (and (= (point) (save-excursion (back-to-indentation) (point)))
	     (not (c-within-comment-p))
	     (save-excursion (beginning-of-line)
			     (not (looking-at "[ \t]*//")))
	     (not (looking-at objc-class-decl))
	     (looking-at (concat objc-feature-decl-or-def
				 objc-comment-regexp "[\{\;,]")))
	(cond ((objc-feature-def-p)
	       (message "(OO-Browser):  Element definition is at the top of the window.")
	       (recenter 0)
	       t)
	      ;; Now look for feature definition in code (non-header) files.
	      ((objc-feature-decl-p)
	       (let ((class) feature-name signature)
		 (setq signature (buffer-substring (match-beginning 1)
						   (match-end 1)))
		 (save-excursion
		   (if (re-search-backward objc-class-def-regexp nil t)
		       (setq class (buffer-substring
				    (match-beginning objc-class-name-grpn) 
				    (match-end objc-class-name-grpn)))))
		 (setq signature (objc-feature-normalize signature class)
		       feature-name (objc-feature-signature-to-name signature))
		 (if (objc-locate-feature feature-name class signature other-win)
		     t
		   (beep)
		   (message "(OO-Browser):  No definition for `%s' in `%s'."
			    feature-name (or class "UNKNOWN-CLASS"))
		   t))))
      (goto-char opoint)
      nil)))

(defun objc-feature-decl-p ()
  "Return t if point is within an Objective-C feature declaration."
  (save-excursion
    (beginning-of-line)
    (looking-at objc-feature-declaration)))

(defun objc-feature-def-p ()
  "Return nil unless point is within the first line of the signature of an element definition.
The definition must be the first thing on the line and must not be commented
or nil is returned."
  (save-excursion
    (objc-skip-to-statement)
    (and (= (point) (save-excursion (back-to-indentation) (point)))
	 (not (c-within-comment-p))
	 (save-excursion (beginning-of-line)
			 (not (looking-at "[ \t]*//")))
	 (not (looking-at objc-class-decl))
	 (looking-at (concat objc-feature-decl-or-def
			     objc-comment-regexp "[\{\;,]"))
	 (eq ?\{ (save-excursion (goto-char (match-end 0))
				 (preceding-char))))))

(defun objc-feature-map-tags (function regexp)
  "Apply FUNCTION to all current feature tags that match REGEXP and return a list of the results."
  (let ((identifier-chars (concat "[" objc-identifier-chars "]*")))
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

(defun objc-feature-matches (regexp)
  "Return an unsorted list of feature tags whose names match in part or whole to REGEXP.
^ and $ characters may be used to match to the beginning and end of a feature name,
respectively."
  (objc-feature-map-tags 'identity regexp))

(defun objc-feature-normalize (routine class &optional unused)
  "Return a feature tag based on ROUTINE and CLASS."
  (setq routine (br-feature-delete-c-comments routine))
  (concat class objc-type-tag-separator
	  (objc-feature-signature-to-name routine nil t)
	  objc-type-tag-separator routine))

(defun objc-files-with-source (class)
  "Use CLASS to compute set of files that match to an Objective-C source file regexp.
Return as a list."
  (let ((file (if class (br-class-path class) buffer-file-name)))
    (and file
	 (let* ((src-file-regexp (concat "^" (br-filename-head file)
					 objc-code-file-regexp))
		(dir (file-name-directory file))
		(files (directory-files dir nil src-file-regexp)))
	   (mapcar (function (lambda (f) (expand-file-name f dir)))
		   files)))))

(defun objc-find-ancestors-feature (class-list signature &optional other-win)
  "Scan ancestors of CLASS-LIST and show routine definition matching SIGNATURE."
  ;; If no class, search for non-element function.
  (or class-list (setq class-list '(nil)))
  (br-feature-display
   class-list (objc-feature-signature-to-regexp signature) other-win))

(defun objc-find-class-name ()
  "Return current word as a potential class name."
  (save-excursion
    (let* ((start)
	   (ignore "-+ \t\n\r\f\;,.<>{}*&\(\)")
	   (pat (concat "^" ignore))
	   name)
      (forward-char 1)
      (skip-chars-backward ignore)
      (skip-chars-backward pat)
      (setq start (point))
      (skip-chars-forward (concat pat ":"))
      (setq name (buffer-substring start (point)))
      ;; Check if name appears within a < > delimited list of names, in which
      ;; case it is a protocol name to which <> delimiters must be added.
      (and (re-search-backward "[<>]" nil t)
	   (eq (following-char) ?\<)
	   (search-forward "\>" nil t)
	   (> (point) start)
	   (setq name (concat "\<" name "\>")))
      name)))

(defun objc-get-class-name-from-source ()
  "Return class name from closest class definition preceding point or nil."
  (let ((opoint (point))
	(class))
    (save-excursion
      (if (re-search-backward objc-class-def-regexp nil t)
	  (progn (setq class (br-buffer-substring
			      (match-beginning objc-class-name-grpn)
			      (match-end objc-class-name-grpn)))
		 ;; Ensure that declaration occurs within class definition.
		 (forward-list)
		 (and (> (point) opoint) class))))))

(defun objc-output-feature-tags (routine-file routine-tags-list)
  "Write Objective-C FEATURE-FILE's ROUTINE-TAGS-LIST into `br-feature-tags-file'.
Assume `br-feature-tags-init' has been called."
  (interactive)
  (save-excursion
    (br-feature-set-tags-buffer)
    (goto-char 1)
    ;; Delete any prior routine tags associated with routine-file.
    (let (start)
      ;; There may be more than one set of entries for each file name.
      (while (search-forward routine-file nil 'end)
	(forward-line -1)
	(setq start (point))
	(search-forward "\^L" nil 'end 2)
	(backward-char 1)
	(delete-region start (point))))
    (if routine-tags-list
	(progn (insert "\^L\n")
	       ;; Quote pathname to avoid read errors on MS OSes.
	       (prin1 routine-file (current-buffer))
	       (insert "\n")
	       (mapcar (function (lambda (tag) (insert tag "\n")))
		       routine-tags-list)))))

(defun objc-include-file (&optional other-win)
  "If point is on an include file line, try to display file.
Return non-nil iff an include file line, even if file is not found.
Look for include file in `objc-cpp-include-dirs' and in directory list
`objc-include-dirs'."
  (let ((opoint (point)))
    (beginning-of-line)
    (if (looking-at objc-include-regexp)
	(let ((incl-type (string-to-char
			  (buffer-substring
			   (match-beginning objc-include-type-grpn)
			   (1+ (match-beginning objc-include-type-grpn)))))
	      (file (buffer-substring
		     (match-beginning objc-include-file-grpn)
		     (match-end objc-include-file-grpn)))
	      (path)
	      (dir-list objc-include-dirs)
	      (found))
	  (goto-char opoint)
	  (setq dir-list (if (eq incl-type ?\<)
			     (append dir-list objc-cpp-include-dirs)
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
			(objc-mode-setup))
		    (br-major-mode))
		(beep)
		(message "(OO-Browser):  Include file `%s' unreadable" path))
	    (beep)
	    (message "(OO-Browser):  Include file `%s' not found" file))
	  path)
      (goto-char opoint)
      nil)))

(defun objc-locate-feature (ftr class signature &optional other-win)
  ;; `class' may = nil, implying non-element function
  (let ((def-class))
    (if (and signature
	     (setq def-class
		   (objc-find-ancestors-feature (list class)
						signature other-win)))
	(progn (if (and class (not (equal class def-class)))
		   (message
		     "Element `%s' of class `%s' inherited from class `%s'."
		     ftr class def-class))
	       t))))

(defun objc-scan-ancestors-feature (class-list ftr-regexp &optional other-win)
  "Display routine definition derived from CLASS-LIST, matching FTR-REGEXP.
Scan files with same base name as class file."
  (let  ((classes class-list)
	 (found-ftr)
	 (code-def-files)
	 (file)
	 (class))
    (if (null class-list)
	nil
      (while (and (not found-ftr) classes)
	(setq class (car classes)
	      code-def-files (objc-files-with-source class))
	(while (and (setq file (car code-def-files))
		    (not (setq found-ftr
			       (br-feature-found-p file ftr-regexp
						   nil other-win t))))
	  (setq code-def-files (cdr code-def-files)))
	(setq classes (if found-ftr nil (cdr classes))))
      (if found-ftr
	  (or class t)
	(objc-scan-ancestors-feature
	 (apply 'append (mapcar (function (lambda (cl) (br-get-parents cl)))
				class-list))
	 ftr-regexp)))))

(defun objc-skip-to-statement ()
  (if (re-search-backward "\\(^\\|[\;{}]\\)[ \t]*" nil t)
      (progn (goto-char (match-end 0))
	     (skip-chars-forward " \t")
	     t)))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defconst objc-code-file-regexp ".\\.[cmCM]$"
  "Regular expression matching a unique part of Objective-C source (non-header) file name and no others.")

(defconst objc-include-regexp
  "[ \t/*]*#[ \t]*\\(import\\|include\\)[ \t]+\\([\"\<]\\)\\([^\"\>]+\\)[\"\>]"
  "Regexp to match to Objective-C include file lines.
File name is grouping `objc-include-file-grpn'.  Type of include,
user-specified via double quote, or system-related starting with `\<' is given
by grouping `objc-include-type-grpn'.")

(defconst objc-include-type-grpn 2)
(defconst objc-include-file-grpn 3)

(defconst objc-type-def-modifier
  "\\(auto\\|const\\|inline\\|register\\|static\\|typedef\\)")

(defconst objc-func-identifier (concat
			       "[_a-zA-Z][^][ \t:;.,~{}()]*")
  "Regular expression matching an Objective-C function name.")

(defconst objc-feature-decl-or-def
  "[-+]\\([^\]\[{}\;`'\"/|?,!.#$%^=+-]+[,. ]*\\)"
  "Regexp matching an Objective-C feature declaration or definition.
Feature name is group 1.")

(defconst objc-feature-name-grpn 1)

(defconst objc-comment-regexp "\\([ \t\n\r\f]*//.*[\n]\\)*[ \t\n\r\f]*")

(defconst objc-routine-def (concat "^" objc-feature-decl-or-def
				   objc-comment-regexp "{"))

(defconst objc-feature-declaration
  (concat "^[ \t]*\\(" objc-feature-decl-or-def "\\)" objc-comment-regexp "\;"))

(defconst objc-class-decl
  (concat objc-class-name-before objc-identifier "[ \t]*")
  "Regexp matching an Objective-C class declaration.
Class name is grouping `objc-class-name-grpn'.")

(defconst objc-class-name-grpn 2)

(defconst objc-arg-identifier (concat
			      "[_a-zA-Z][" objc-identifier-chars "]*")
  "Regular expression matching an Objective-C function argument identifier.")

(defconst objc-name-part-prefix
  "[ \t\n\r]*\\(([^\)]+)[ \t\n\r]*\\)?")

(defconst objc-name-part
  (concat objc-name-part-prefix objc-identifier "?"))

(defconst objc-name-sep "[ \t\n\r]*\\([:\;\{]\\)")

(defconst objc-type-sep "\\([ \t\n\r\;\{]\\)")

(defconst objc-name-part-type 1)
(defconst objc-name-part-id 2)
(defconst objc-name-part-sep 3)

(provide 'br-objc-ft)
