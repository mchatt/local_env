;;!emacs
;;
;; FILE:         eif-calls.el
;; SUMMARY:      Produce first level static call tree for Eiffel class.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:     7-Dec-89 at 19:32:47
;; LAST-MOD:     10-May-01 at 13:14:11 by Bob Weiner
;;
;; Copyright (C) 1989-1995, 1997  BeOpen.com
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;   The default commands, `eif-store-class-info' and `eif-insert-class-info'
;;     work in tandem to display the parents, attributes and routines with
;;     routine call summaries for a class.
;;   The command {M-x eif-info-use-short}, will instead cause the above
;;     commands to run the Eiffel `short' command on a class, thereby
;;     displaying its specification.
;;   The command {M-x eif-info-use-flat}, will instead cause the above
;;     commands to run the Eiffel `flat' command on a class, thereby
;;     displaying its complete feature set.
;;   Call {M-x eif-info-use-calls} to reset these commands to their default.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'br-eif)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun eif-info-use-calls ()
  "Setup to display call trees and other class summary info."
  (interactive)
  (defalias 'eif-store-class-info  'eif-store-class-info-calls)
  (defalias 'eif-insert-class-info 'eif-insert-class-info-calls))
(eif-info-use-calls)

(defun eif-info-use-flat ()
  "Setup to display the Eiffel `flat' output for classes."
  (interactive)
  (defalias 'eif-store-class-info  'eif-store-class-info-flat)
  (defalias 'eif-insert-class-info 'eif-insert-class-info-flat))

(defun eif-info-use-short ()
  "Setup to display the Eiffel `short' output for classes."
  (interactive)
  (defalias 'eif-store-class-info  'eif-store-class-info-short)
  (defalias 'eif-insert-class-info 'eif-insert-class-info-short))

(defun eif-show-class-info (&optional class-name)
  "Displays class specific information summary in other window.
This summary includes listings of textually included attributes, routines,
and routine calls from an Eiffel class.  Use optional CLASS-NAME for class
text or extract from the current buffer."
  (interactive (list (br-complete-class-name
		      nil
		      (let ((cn (car (eif-get-class-name-from-source))))
			(if cn (concat "Class name: (default " cn ") "))))))
  (let ((class-file-name))
    (if (not (br-class-in-table-p class-name))
	(if (setq class-file-name buffer-file-name)
	    (setq class-name (car (eif-get-class-name-from-source)))
	  (error "No class specified.")))
    (if (null class-name)
	(error "No class specified.")
      (message "Building `%s' class info..." class-name)
      (sit-for 1)
      (eif-store-class-info class-name)
      (message "Building `%s' class info...Done" class-name)
      (pop-to-buffer "*Class Info*")
      (eif-insert-class-info class-file-name))))

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(defun eif-get-class-name-from-source ()
  "Return indication of closest class definition preceding point or nil.
If non-nil, value is a cons cell of (class-name . deferred-class-p)."
  (save-excursion
    (if (or (re-search-backward eif-class-def-regexp nil t)
	    (re-search-forward eif-class-def-regexp nil t))
	(cons (br-buffer-substring (match-beginning 2)
				   (match-end 2))
	      (match-end 1)))))

(defun eif-insert-class-info-calls (&optional src-file-name)
  "Inserts textually included attributes, routines, and routine calls from `eif-last-class-name'.
Uses optional SRC-FILE-NAME for lookups or class name from `eif-last-class-name'."
  (interactive)
  (cond ((and eif-last-class-name (null eif-attributes-and-routines))
	 (eif-store-class-info eif-last-class-name))
	((and eif-last-class-name eif-attributes-and-routines)
	 nil)
	(t (error
	    (concat "Call `eif-store-class-info' first."
		    (let ((key (car (where-is-internal 'eif-store-class-info))))
		      (and key (concat "  It is bound to {" key "}.")))))))
  (let ((in-lookup-table 
	  (if src-file-name
	      nil
	    (br-class-in-table-p eif-last-class-name))))
    (if (not (or in-lookup-table src-file-name))
	nil
      (insert eif-last-class-name)
      (center-line)
      (insert "\n")
      (insert "Parents:\n")
      (let ((parents (if in-lookup-table
			 (br-get-parents eif-last-class-name)
		       (eif-get-parents-from-source src-file-name nil))))
	(if parents
	    (mapcar (function (lambda (par) (insert "   " par "\n")))
		    parents)
	  (insert "   <None>\n"))
	(let ((attribs (car eif-attributes-and-routines))
	      (routines (cdr eif-attributes-and-routines)))
	  (if parents
	      (insert "\nNon-Inherited Attributes:\n")
	    (insert "\nAttributes:\n"))
	  (if attribs
	      (mapcar (function (lambda(attr) (insert "   " attr "\n")))
		      attribs)
	    (insert "   <None>\n"))
	  (if parents
	      (insert
	       "\nNon-Inherited Routines with Apparent Routine Calls:\n")
	    (insert "\nRoutines with Apparent Routine Calls:\n"))
	  (if routines
	      (mapcar (function
			(lambda(cns)
			  (insert "   " (car cns) "\n")
			  (mapcar (function
				    (lambda (call)
				     (insert "      " call "\n")))
				  (cdr cns))))
		      routines)
	    (insert "   <None>\n"))
	  ))
      (set-buffer-modified-p nil))))

(defun eif-store-class-info-calls (class-name)
  "Generates cons of textually included attributes and routines (including routine calls) from CLASS-NAME.
It stores this cons in the global `eif-attributes-and-routines'."
  (interactive (list (br-complete-class-name)))
  (setq eif-last-class-name class-name)
  (let ((in-lookup-table (br-class-path eif-last-class-name)))
    (if (not (or in-lookup-table buffer-file-name))
	nil
      (setq eif-attributes-and-routines
	    (eif-get-features-from-source
	      (if in-lookup-table
		  (br-class-path eif-last-class-name)
		buffer-file-name))))))

(defun eif-insert-class-info-short ()
  (interactive)
  (insert-file-contents eif-tmp-info-file)
  (shell-command (concat "rm -f " eif-tmp-info-file))
  (message ""))

(defun eif-store-class-info-short (class-name)
  (interactive (list (br-complete-class-name)))
  (shell-command (concat "short -b 3 -p "
			 (br-class-path (br-find-class-name))
			 "> " eif-tmp-info-file)))

(defun eif-insert-class-info-flat ()
  (interactive)
  (insert-file-contents eif-tmp-info-file)
  (shell-command (concat "rm -f " eif-tmp-info-file))
  (message ""))

(defun eif-store-class-info-flat (class-name)
  (interactive (list (br-complete-class-name)))
  (shell-command (concat "flat -b 3 "
			 (br-class-path (br-find-class-name))
			 "> " eif-tmp-info-file)))

(defun eif-class-name-from-file-name (file-name)
  (string-match "^.*/\\([a-z0-9_]+\\)\\.e$" file-name)
  (if (match-beginning 1)
      (substring file-name (match-beginning 1) (match-end 1))))

(defun eif-eval-in-other-window (buffer form)
  "Clear out BUFFER and display result of FORM evaluation in viewer window.
Then return to previous window.  BUFFER may be a buffer name."
  (interactive)
  (let ((wind (selected-window)))
    (pop-to-buffer (get-buffer-create buffer))
    (let (buffer-read-only)
      (erase-buffer)
      (eval form))
    (goto-char (point-min))
    (setq buffer-read-only t)
    (select-window wind)))

(defun eif-get-attribute-definition-regexp (identifier-regexp)
  "Return regexp to match to IDENTIFIER-REGEXP definition.
Matching attribute name is grouping `eif-feature-name-grpn'.
Additional attributes in the same declaration, if any, are matched
by grouping `eif-feature-multiple-names-grpn'."
  (concat eif-modifier-regexp
	  "\\(" identifier-regexp "\\)"
	  "\\([ \t]*,[ \t\n\r]*" eif-identifier "\\)*"
	  "[ \t]*:[ \t]*"
	  eif-type "\\([ \t]+is[ \t]+.+\\)?[ \t]*;?[ \t]*\\(--.*\\)?$"))

(defun eif-get-features-from-source (filename &optional form)
  "Returns cons of attribute def list and routine def list from Eiffel class FILENAME.
Optional FORM is a Lisp form to be evaluated instead of the default feature
extraction.  Assumes file existence has already been checked.  The cdr of
each element of each item in routine def list is a best guess list of
subroutines invoked by the routine."
  (let* ((no-kill (get-file-buffer filename))
	 (tmp-buf (set-buffer (get-buffer-create "*tmp*")))
	 feature-list orig-buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (if no-kill
	(set-buffer no-kill)
      (setq orig-buf (funcall br-find-file-noselect-function filename))
      (set-buffer orig-buf))
    (copy-to-buffer tmp-buf (point-min) (point-max))
    (set-buffer tmp-buf)
    (goto-char (point-min))
    (while (re-search-forward "^\\([^\"\n]*\\)--.*" nil t)
      (replace-match "\\1" t nil))
    (goto-char (point-min))
    (if (not (re-search-forward "^feature[ \t]*$" nil t))
	nil
      (setq feature-list
	    (if form
		(eval form)
	      (eif-parse-features)))
      (erase-buffer)			; tmp-buf
      (or no-kill (kill-buffer orig-buf))
      )
    feature-list))

(defun eif-in-comment-p ()
  "Return nil unless point is within an Eiffel comment."
  (save-excursion
    (let ((end (point)))
      (beginning-of-line)
      (search-forward "--" end t))))

(defun eif-to-attribute (&optional identifier)
  "Move point to attribute matching optional IDENTIFIER or next attribute def in buffer.
Leave point at beginning of line where feature is defined.
Return name of attribute matched or nil.  Ignore obsolete attributes."
  (let ((pat (if identifier
		 (eif-attribute-to-regexp identifier)
	       eif-attribute-regexp))
	(start)
	(found)
	(keyword)
	(non-attrib-keyword "local\\|require\\|ensure\\|invariant"))
    (while (and (re-search-forward pat nil t)
		(setq found (buffer-substring 
			     (match-beginning eif-attribute-name-grpn)
			     (match-end eif-attribute-name-grpn))
		      start (match-beginning 0))
		;; Continue loop if in a comment or a local declaration.
		(or (if (eif-in-comment-p)
			(progn (setq found nil) t))
		    (save-excursion
		      (while (and (setq keyword
					(re-search-backward
					 (concat
					  "\\(^\\|[ \t]+\\)\\("
					  "end\\|feature\\|"
					  non-attrib-keyword
					  "\\)[\; \t\n\r]")
					 nil t))
				  (eif-in-comment-p)))
		      (if (and keyword
			       (setq keyword
				     (buffer-substring
				      (match-beginning 2)
				      (match-end 2)))
			       (equal 0 (string-match non-attrib-keyword
						      keyword)))
			  (progn (setq found nil) t))))))
    (if start (goto-char start))
    found))

(defun eif-parse-attributes ()
  "Returns list of attributes defined in current buffer.
Each attribute contains its listing display prefix.
Assumes point is at the start of buffer."
  (let ((attribs) attrib multiple-attribs len start)
    ;; For each attribute definition (may be a list of attributes)
    (while (and (eif-to-attribute)
		(looking-at eif-attribute-regexp))
      (setq attrib (buffer-substring
		    (match-beginning eif-feature-name-grpn)
		    (match-end eif-feature-name-grpn))
	    multiple-attribs
	    (concat attrib
		    (if (match-beginning eif-feature-multiple-names-grpn)
			(buffer-substring
			 (match-beginning
			  eif-feature-multiple-names-grpn)
			 (match-end eif-feature-multiple-names-grpn)))))
      (goto-char (match-end 0))

      (setq len (length multiple-attribs)
	    start 0)
      (while (and (< start len)
		  (string-match (concat ",?[ \t\n\r]*" eif-identifier)
				multiple-attribs start))
	(setq start (match-end 0)
	      attrib (substring multiple-attribs (match-beginning 1)
				(match-end 1)))
	(if (or (> (length attrib) 9)
		(< (length attrib) 2))
	    nil
	  (if (hash-key-p attrib eif-reserved-words-htable)
	      (setq attrib nil)))
	(if attrib
	    (progn (setq attrib (concat "= " attrib))
		   (br-set-cons attribs attrib)))))
    (setq attribs (nreverse attribs))))

(defun eif-parse-features (&optional skip-calls)
  "Returns cons of attribute def list and routine def list from current buffer.
The cdr of each item in routine def list is a best guess list of routine calls
invoked by the routine, unless optional SKIP-CALLS is non-nil, in which case
each item is just the routine name."
  (let ((routines) attribs calls external len multiple-routines non-ids 
	reserved routine (start 0) (type))
    ;; Get attribute definitions
    ;; and add attributes to list of names not to consider routine invocations.
    (setq attribs (eif-parse-attributes)
	  non-ids (append attribs eif-reserved-words))
    (goto-char (point-min))
    ;; For each routine definition (may be a list of routines):
    (while (re-search-forward eif-routine-regexp nil t)
      (setq routine (buffer-substring
		     (match-beginning eif-feature-name-grpn)
		     (match-end eif-feature-name-grpn))
	    multiple-routines
	    (concat routine
		    (if (match-beginning eif-feature-multiple-names-grpn)
			(buffer-substring
			 (match-beginning
			  eif-feature-multiple-names-grpn)
			 (match-end eif-feature-multiple-names-grpn))))
	    external (if (match-beginning eif-modifier-grpn)
			 (string-match "external"
				       (buffer-substring
					(match-beginning eif-modifier-grpn)
					(match-end eif-modifier-grpn))))
	    reserved non-ids)
      (if (match-beginning eif-feature-args-grpn)
	  ;; Routine takes a list of arguments.
	  ;; Add ids matched to list of names not to consider routine
	  ;; invocations.
	  (setq reserved
		(append (eif-parse-params
			 (match-beginning eif-feature-args-grpn)
			 (match-end eif-feature-args-grpn))
			reserved)))

      (if (and (not external)
	       (re-search-forward
		"^[ \t]*\\(do\\|once\\|deferred\\|external\\)[ \t\n\r]+"
		nil t))
	  (setq type (buffer-substring (match-beginning 1) (match-end 1))
		type (cond ((string-equal type "do") "- ")
			   ((string-equal type "once") "1 ")
			   ((string-equal type "external") "/ ")
			   (t ;; deferred type
			    "> "))
		calls (if skip-calls nil (nreverse (eif-parse-ids reserved)))))

      (setq len (length multiple-routines)
	    start 0)
      (while (and (< start len)
		  (string-match (concat ",?[ \t\n\r]*" eif-routine-name-regexp)
				multiple-routines start))
	(setq start (match-end 0)
	      routine (substring multiple-routines (match-beginning 1)
				 (match-end 1)))
	(cond (external
	       (setq routine (concat "/ " routine)))
	      (type
	       (setq routine (concat type routine))))
	(if skip-calls
	    (setq routines (cons routine routines))
	  (setq routines (cons (cons routine calls) routines)))))
    (setq routines (nreverse routines))
    (cons attribs routines)))
    
(defun eif-parse-ids (&optional non-ids)
  "Ignores list of NON-IDS and returns list of Eiffel identifiers through the end of the current routine definition."
  (let (call calls lcall call-list non-id-list same start valid-call)
    (while (and (setq start (eif-try-for-routine-call))
		;; Ignore assignable entities
		(cond ((stringp start)
		       (setq non-ids (cons (downcase start) non-ids)))
		      ;; Ignore reserved word expressions that look like
		      ;; routine calls with arguments
		      ((and (setq call
				  (downcase
				    (buffer-substring start (match-end 0))))
			    (looking-at "[ \t]*\(")
			    (br-member call non-ids)))
		      ;; Skip past rest of this routine invocation
		      ((progn
			 (while (or (progn (setq valid-call t same (point))
					   (and (setq call
						      (eif-skip-past-arg-list)
						      valid-call
						      (or (null call)
							  (= call 0)))
						(looking-at "\\.")
						(progn
						  (skip-chars-forward ".")
						  (if (setq valid-call
							    (looking-at
							     eif-identifier))
						      (goto-char
						       (match-end 0)))))
					   (> (point) same))
				    (if (and valid-call (looking-at "\\."))
					(progn (skip-chars-forward ".")
					       (if (setq valid-call
							 (looking-at
							   eif-identifier))
						   (goto-char
						    (match-end 0)))))))
			 (if (and valid-call
				  (/= start (point)))
			     (progn (setq call (buffer-substring start (point))
					  lcall (downcase call))
				    ;; If at end of `do' part of routine
				    ;; definition...
				    (if (or (string-equal lcall "ensure")
					    (and (string-equal lcall "end")
						 (looking-at
						   "[ \t]*\;?[ \t\r]*[\n][ \t\r]*[\n]")))
					(setq valid-call nil)
				      (if call (br-set-cons calls call))
				      )
				    valid-call)
			   nil))))))
    (while calls
      (setq call (car calls)
	    calls (cdr calls)
	    lcall (downcase call)
	    non-id-list
	    (or non-ids eif-reserved-words))
      (if (br-member lcall non-id-list)
	  (setq call nil))
      (if call (setq call-list (append call-list (list call)))))
    call-list))

(defun eif-parse-params (start end)
  "Returns list of Eiffel formal parameters between START and END, in reverse order."
  (narrow-to-region start end)
  (goto-char (point-min))
  (let (params)
    (while (re-search-forward eif-identifier nil t)
      (setq params (cons (buffer-substring
			  (match-beginning 0) (match-end 0)) params))
      (if (looking-at "[ \t]*:")
	  (progn (goto-char (match-end 0))
		 (re-search-forward eif-type nil t)))
      )
    (widen)
    params))

(defun eif-skip-past-arg-list ()
  "Skips path arg list delimited by parenthesis.
Leaves point after closing parenthesis.  Returns number of unclosed parens
iff point moves, otherwise nil." 
  (let ((depth 0))
    (if (not (looking-at "[ \t]*\("))
	nil
      (setq depth (1+ depth))
      (goto-char (match-end 0))
      (while (> depth 0)
	(skip-chars-forward "^()\"'")
	(cond ((eq ?\" (following-char))
	       (progn (forward-char 1)
		      (skip-chars-forward "^\"")))
	      ((eq ?' (following-char))
	       (progn (forward-char 1)
		      (skip-chars-forward "^'")))
	      ((setq depth (if (eq ?\( (following-char))
			      (1+ depth)
			    (1- depth)))))
	(and (not (eobp)) (forward-char 1)))
      depth)))

(defun eif-try-for-routine-call ()
  "Matches to best guess of next routine call.
Returns character position of start of valid match, nil when no match,
identifier string when an assignable entity, i.e. matches to a non-routine."
  (if (re-search-forward (concat eif-identifier "\\([ \t\n\r]*:=\\)?") nil t)
      (if (match-beginning 2)
	  (buffer-substring (match-beginning 1) (match-end 1))
	(match-beginning 0))))

;;; ************************************************************************
;;; Internal variables
;;; ************************************************************************

(defvar eif-reserved-words
  '("!!" "alias" "and" "as" "bits" "boolean" "character" "check" "class"
    "clone" "create" "creation" "current" "debug" "deferred" "define" "div"
    "do" "double" "else" "elseif" "end" "ensure" "expanded" "export"
    "external" "false" "feature" "forget" "from" "if" "implies" "indexing"
    "infix" "inherit" "inspect" "integer" "invariant" "is" "language" "like"
    "local" "loop" "mod" "name" "nochange" "not" "obsolete" "old" "once" "or"
    "prefix" "real" "redefine" "rename" "repeat" "require" "rescue" "result"
    "retry" "select" "then" "true" "undefine" "unique" "until" "variant"
    "void" "when" "xor")
  "Lexicographically ordered list of reserved words in Eiffel version 2.2.
Longest one is 9 characters.
Minor support for Eiffel 3 has now been added.")

(defvar eif-reserved-words-htable
  (hash-make (mapcar 'list eif-reserved-words) t))

;; Must handle types of these forms:
;;   like LIST [INTEGER]
;;   VECTOR [INTEGER , INTEGER]
;;   LIST [ LIST[INTEGER]]
;; yet must ignore the `is' in:
;;   var: INTEGER is 0
(defconst eif-type
  "\\(like[ \t]+\\)?[a-zA-Z][a-zA-Z_0-9]*\\([ \t]*\\[.+\\]\\)?"
  "Regexp to match Eiffel entity and return value type expressions.")

(defconst eif-modifier-regexp
  "^[ \t]*\\(frozen[ \t\n\r]+\\|external[ \t]+\"[^\" ]+\"[ \t\n\r]+\\)?"
  "Special prefix modifiers that can precede a feature definition.")

;; Handles attributes of these forms:
;;   attr: TYPE
;;   char: CHARACTER is 'a'
;;   message: STRING is "Hello, what is your name?"
;;   flag: BOOLEAN is true ;
(defconst eif-attribute-regexp
  (eif-get-attribute-definition-regexp eif-identifier)
  "Regexp to match to an attribute definition line.")

(defconst eif-routine-name-regexp
  (concat "\\(" eif-identifier
	  "\\|prefix[ \t]+\"[^\" ]+\"\\|infix[ \t]+\"[^\" ]+\"\\)")
  "Regexp matching the name of an Eiffel routine.
The whole regexp is treated as a grouping.")

(defconst eif-routine-regexp
  (concat eif-modifier-regexp eif-routine-name-regexp "[ \t\n\r]*"
	  "\\(,[ \t\n\r]*" eif-routine-name-regexp "[ \t\n\r]*\\)*"
	  "\\(([^\)]+)[ \t]*\\)?"
	  "\\(:[ \t\n\r]*"
	  eif-type "[ \t\n\r]+\\)?is[ \t\r]*$")
  "Regexp to match to a routine definition line (including definition lists).
Ignores obsolete routines.")

(defun eif-attribute-to-regexp (identifier)
  "Return regexp to match to IDENTIFER attribute definition.
Attribute name is grouping `eif-feature-name-grpn'."
  (concat eif-modifier-regexp
	  "\\(" eif-identifier "[ \t]*,[ \t\n\r]*\\)*"
	  "\\(" (regexp-quote identifier) "\\)"
	  "\\([ \t]*,[ \t\n\r]*" eif-identifier "\\)*[ \t]*:[ \t\n\r]*"
	  eif-type "\\([ \t\n\r]+is[ \t\n\r]+.+\\)?[ \t]*;?[ \t]*\\(--.*\\)?$"))

(defun eif-routine-to-regexp (identifier)
  "Return regexp to match to IDENTIFIER's routine definition.
Ignore obsolete routines."
  (concat eif-modifier-regexp
	  "\\(" eif-routine-name-regexp "[ \t]*,[ \t\n\r]*\\)*"
	  "\\(" (regexp-quote identifier) "\\)"
	  "\\([ \t]*,[ \t\n\r]*" eif-routine-name-regexp "\\)*[ \t\n\r]*"
	  "\\(([^\)]+)[ \t\n\r]*\\)?\\(:[ \t\n\r]*"
	  eif-type "[ \t\n\r]+\\)?is[ \t\n\r]*\\(--.*\\)?$"))

(defconst eif-modifier-grpn 1
  "Regexp grouping for leading feature modifies, `frozen' or `external'.")

(defconst eif-feature-name-grpn 2
  "Regexp grouping for feature name from `eif-attribute-regexp',
`eif-routine-regexp' or (eif-attribute-to-regexp).")

(defconst eif-attribute-name-grpn 4
  "Regexp grouping for feature name from (eif-attribute-to-regexp).")

(defconst eif-feature-multiple-names-grpn 4
  "Regexp grouping for 2 or more feature names matched by `eif-attribute-regexp' or `eif-routine-regexp'.")

(defconst eif-feature-args-grpn 7
  "Regexp grouping for feature arg list matched by `eif-routine-regexp'.")

(defvar eif-last-class-name nil
  "Last class name used as parameter to `eif-store-class-info'.  Value is
used by `eif-insert-class-info'.")

(defvar eif-attributes-and-routines nil
  "Class data stored by `eif-store-class-info' for use by `eif-insert-class-info'.")

(defconst eif-tmp-info-file (expand-file-name
			     (concat (user-real-login-name) "-eif-info")
			     (br-temp-directory))
  "Temporary file used to hold Eiffel class info.")

(provide 'eif-calls)
