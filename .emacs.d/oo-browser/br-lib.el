;;!emacs
;;
;; FILE:         br-lib.el
;; SUMMARY:      OO-Browser support functions.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:    22-Mar-90
;; LAST-MOD:     10-May-01 at 15:17:10 by Bob Weiner

;;; ************************************************************************
;;; Inline functions
;;; ************************************************************************

(defsubst br-get-children-htable ()
  "Loads or builds `br-children-htable' if necessary and returns value."
  (br-get-htable "children"))

(defsubst br-get-paths-htable ()
  "Loads or builds `br-paths-htable' if necessary and returns value."
  (br-get-htable "paths"))

(defsubst br-get-parents-htable ()
  "Loads or builds `br-parents-htable' if necessary and returns value."
  (br-get-htable "parents"))

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'br-ftr)
(require 'br-compl)
(require 'set)
(require 'hasht)
(require 'hpath)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar br-null-path "<none>"
  "Pathname associated with OO-Browser entities which have no source file.
That is, virtual entities, such as categories.")

(defvar br-empty-htable (hash-make 0)
  "An empty hash table used to check whether OO-Browser hash tables are empty.")

;;; ************************************************************************
;;; General public functions
;;; ************************************************************************

(defun br-add-default-classes (class-list)
  "Add classes from CLASS-LIST as default classes for the current Environment.
Default class names should be surrounded by square brackets.
Add classes to the list of System classes."
  (mapcar (function (lambda (class)
		      (br-add-class class br-null-path nil)))
	  class-list))

(defun br-buffer-replace (regexp to-str &optional literal-flag)
  "In current buffer, replace all occurrences of REGEXP with TO-STR.
Optional LITERAL-FLAG non-nil means ignore special regexp characters
and just insert them literally."
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (replace-match to-str t literal-flag)
    (backward-char 1)))

(if (fboundp 'buffer-substring-no-properties)
(defalias 'br-buffer-substring 'buffer-substring-no-properties)
(defalias 'br-buffer-substring 'buffer-substring))

(defun br-buffer-delete-c-comments ()
  "Remove all // and /* */ comments from the current buffer.
Assumes the buffer is not read-only and does not handled nested
multi-line comments."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "//.*" nil t) (replace-match "" t t))
    (goto-char (point-min))
    (while (re-search-forward "/\\*" nil t)
      (delete-region (match-beginning 0)
		     (if (re-search-forward "\\*/" nil t)
			 (match-end 0)
		       (point-max))))))

(defun br-c-to-comments-begin ()
  "Skip back from current point past any preceding C-based comments at the beginning of lines.
Presumes no \"/*\" strings are nested within multi-line comments."
  (let ((opoint))
    (while (progn (setq opoint (point))
		  ;; To previous line
		  (if (zerop (forward-line -1))
		      (cond
		       ;; If begins with "//" or ends with "*/", then is a
		       ;; comment.
		       ((looking-at "[ \t]*\\(//\\|$\\)"))
		       ((looking-at ".*\\*/[ \t]*$")
			(progn (end-of-line)
			       ;; Avoid //*** single line comments here.
			       (if (re-search-backward "\\(^\\|[^/]\\)/\\*" nil t)
				   (progn (beginning-of-line)
					  (looking-at "[ \t]*/\\*")))))
		       (t nil)))))
    (goto-char opoint)
    ;; Skip past whitespace
    (skip-chars-forward " \t\n\r\f")
    (beginning-of-line)))

(defun br-delete-space (string)
  "Delete any leading and trailing space from STRING and return the STRING."
  (if (string-match "\\`[ \t\n\r\f]+" string)
      (setq string (substring string (match-end 0))))
  (if (string-match "[ \t\n\r\f]+\\'" string)
      (setq string (substring string 0 (match-beginning 0))))
  string)

(defun br-first-match (regexp list)
  "Return non-nil if REGEXP matches to an element of LIST.
All elements of LIST must be strings.
The value returned is the first matched element."
  (while (and list (not (string-match regexp (car list))))
    (setq list (cdr list)))
  (car list))

(defun br-filename-head (path)
  (regexp-quote (file-name-sans-extension
		 (file-name-nondirectory path))))

(defun br-flatten (obj)
  "Return a single-level list of all atoms in OBJ in original order.
OBJ must be a list, cons cell, vector or atom."
  ;; Test case:   (br-flatten '((a b (c . d)) e (f (g [h (i j) [k l m]] (n)))))
  ;; Should produce: => (a b c d e f g h i j k l m n)
  (cond ((null obj) nil)
	((vectorp obj)
	 ;; convert to list
	 (setq obj (append obj nil))
	 ;; flatten new list
	 (append (br-flatten (car obj)) (br-flatten (cdr obj))))
	((atom obj) (list obj))
	((nlistp obj)
	 (error "(br-flatten): Invalid type, '%s'" obj))
	(t ;; list
	 (append (br-flatten (car obj)) (br-flatten (cdr obj))))))

(defun br-duplicate-and-unique-strings (sorted-strings)
  "Return SORTED-STRINGS list with a list of duplicate entries consed onto the front of the list."
  (let ((elt1) (elt2) (lst sorted-strings)
	(count 0) (repeat) (duplicates))
    (while (setq elt1 (car lst) elt2 (car (cdr lst)))
      (cond ((not (string-equal elt1 elt2))
	     (setq lst (cdr lst)))
	    ((equal elt1 repeat)
	    ;; Already recorded this duplicate.
	     (setcdr lst (cdr (cdr lst))))
	    (t ;; new duplicate
	     (setq count (1+ count)
		   duplicates (cons elt1 duplicates)
		   repeat elt1)
	     (setcdr lst (cdr (cdr lst))))))
    (cons (sort duplicates 'string-lessp) sorted-strings)))

(defun br-set-of-strings (sorted-strings &optional count)
  "Return SORTED-STRINGS list with any duplicate entries removed.
Optional COUNT conses number of duplicates on to front of list before return."
  (and count (setq count 0))
  (let ((elt1) (elt2) (lst sorted-strings)
	(test (if count
		  (function
		    (lambda (a b) (if (string-equal a b)
				      (setq count (1+ count)))))
		(function (lambda (a b) (string-equal a b))))))
    (while (setq elt1 (car lst) elt2 (car (cdr lst)))
      (if (funcall test elt1 elt2)
	  (setcdr lst (cdr (cdr lst)))
	(setq lst (cdr lst)))))
  (if count (cons count sorted-strings) sorted-strings))

(defun br-member-sorted-strings (elt list)
  "Return non-nil if ELT is an element of LIST.  Comparison done with `string-equal'.
All ELTs must be strings and the list must be sorted in ascending order.
The value returned is actually the tail of LIST whose car is ELT."
  (while (and list (not (string-equal (car list) elt)))
    (setq list (and (string-lessp (car list) elt)
		    (cdr list))))
  list)

(defun br-delete-sorted-strings (elt set)
  "Removes element ELT from SET and returns new set.
Assumes SET is a valid set of sorted strings.
Use (setq set (br-delete-sorted-strings elt set)) to assure that the set is
always properly modified." 
  (let ((rest (br-member-sorted-strings elt set)))
    (if rest
	(cond ((= (length set) 1) (setq set nil))
	      ((= (length rest) 1)
	       (setcdr (nthcdr (- (length set) 2) set) nil))
	      (t (setcar rest (car (cdr rest)))
		 (setcdr rest (cdr (cdr rest)))))))
  set)

(defun br-pathname-head (path)
  (if (string-match "\\(.+\\)\\." path)
      (substring path 0 (match-end 1))
    path))

(defun br-quote-match (match-num)
  "Quote special symbols in last matched expression MATCH-NUM."
  (br-regexp-quote (br-buffer-substring (match-beginning match-num)
					(match-end match-num))))

(defun br-rassoc (elt list)
  "Return non-nil if ELT is the cdr of an element of LIST.
Comparison done with `equal'.  The value is actually the tail of LIST
starting at the element whose cdr is ELT."
  (while (and list (not (equal (cdr (car list)) elt)))
    (setq list (cdr list)))
  list)

(defun br-regexp-quote (obj)
  "If OBJ is a string, quote and return it for use in a regular expression."
  ;; Don't use (stringp obj) here since we want to signal an error if some
  ;; caller ever passes in a non-nil, non-string object, to aid in debugging.
  (if obj (regexp-quote obj)))

(defun br-relative-path (filename &optional directory)
  "Convert FILENAME to be relative to DIRECTORY or default-directory.
The shorter of the absolute and relative paths is returned."
  (let ((relative-path (file-relative-name filename directory)))
    (if (< (length relative-path) (length filename))
	relative-path
      filename)))

(defmacro br-set-cons (set elt)
  "Add to SET element ELT.  Returns nil iff ELT is already in SET.
Uses `equal' for comparison."
  (` (if (br-member (, elt) (, set))
	 nil
       (setq (, set) (cons (, elt) (, set))))))

(if (fboundp 'temp-directory)
(defalias 'br-temp-directory 'temp-directory)
(defun br-temp-directory ()
  (let ((tmp-dir
	 (file-name-as-directory
	  (or (getenv "TMPDIR")
	      (getenv "TMP")
	      (getenv "TEMP")
	      (if (file-directory-p "/tmp/") "/tmp/")
	      (if (file-directory-p "C:/tmp/") "C:/tmp/")
	      (if hyperb:microcruft-os-p
		  (condition-case ()
		      (progn (make-directory "C:/tmp/") "C:/tmp/")
		    (error (expand-file-name "~")))
		(condition-case ()
		    (progn (make-directory "/tmp/") "/tmp/")
		  (error (expand-file-name "~"))))))))
    (if (file-writable-p tmp-dir)
	(progn (setenv "TMPDIR" tmp-dir) tmp-dir)
      (error "(br-temp-directory): Temp dir, \"%s\", is not writable."
	     tmp-dir)))))

(defun br-wind-line-at-point ()
  "Return window relative line number that point is on."
  (max 0 (1- (- (count-lines 1 (min (1+ (point)) (point-max)))
		(count-lines 1 (window-start))))))

;;; ************************************************************************
;;; Browser public functions
;;; ************************************************************************

(defun br-add-class (class-name &optional class-path lib-table-p save-file)
  "Add or replace CLASS-NAME in current Environment.
  Find class source in optional CLASS-PATH.  Interactively or when optional
CLASS-PATH is nil, defaults to current buffer file as CLASS-PATH.  If
optional LIB-TABLE-P is non-nil, add to Library Environment, otherwise add to
System Environment.  If optional SAVE-FILE is t, the Environment is then
stored to filename given by `br-env-file'.  If SAVE-FILE is non-nil and
not t, its string value is used as the file to which to save the Environment.
Does not update children lookup table."
  (interactive
   (list (read-string "Class name to add: ")
	 (read-file-name (concat "Class file name"
				 (if buffer-file-name
				     " (default <current file>)")
				 ": ")
			 nil buffer-file-name t)
	 (y-or-n-p "Add to Library, rather than System tables? ")
	 (y-or-n-p
	  (concat "Save tables after addition to " br-env-file "? "))))
  ;; 
  ;; Pseudo code:
  ;; 
  ;;    If class-name is in table
  ;;       If function called interactively
  ;;          Query whether should overwrite class-name in tables
  ;;          If yes
  ;;             Replace class and its features
  ;;          else
  ;;             Don't add class; do nothing
  ;;          end
  ;;       else
  ;;          Store class without its features in all necessary tables
  ;;       end
  ;;    else
  ;;       Store class and its features under key in all necessary tables
  ;;    end
  ;;
  (or class-path (setq class-path buffer-file-name)
      (error "No class pathname specified."))
  (if (or (string-equal class-name "")
	  (not (or (equal class-path br-null-path)
		   (file-exists-p class-path))))
      (error "Invalid class specified, `%s', in: \"%s\"" class-name class-path))
  ;; Is class already in Environment?
  (if (hash-key-p class-name (br-get-htable
			      (if lib-table-p "lib-parents" "sys-parents")))
      (if (or (not (interactive-p))
	      (y-or-n-p (format "Overwrite existing `%s' entry? " class-name)))
	  (br-real-add-class lib-table-p class-name class-path t)
	(setq save-file nil))
    (br-real-add-class lib-table-p class-name class-path))
  (cond ((eq save-file nil))
	((eq save-file t) (br-env-save))
	((br-env-save save-file))))

(defun br-build-lib-htable ()
  "Build Library dependent Environment."
  (interactive)
  (cond	((and (interactive-p)
	       (not (y-or-n-p "Rebuild Library Environment? ")))
	 nil)
	(t
	 (message "Building Library Environment...")
	 (sit-for 2)
	 (br-real-build-alists br-lib-search-dirs)
	 (if (interactive-p) (br-feature-build-htables))
	 (setq br-lib-paths-htable (hash-make br-paths-alist)
	       br-lib-parents-htable (hash-make br-parents-alist))
	 (run-hooks 'br-after-build-lib-hook)
	 (br-env-set-htables)
	 ;; Set prev-search-dirs so table rebuilds are not triggered.
	 (setq br-lib-prev-search-dirs br-lib-search-dirs)
	 (if (interactive-p) (br-env-save))
	 (message "Building Library Environment...Done")
	 t)))

(defun br-build-sys-htable ()
  "Build System dependent class Environment."
  (interactive)
  (cond	((and (interactive-p)
	      (not (y-or-n-p "Rebuild System Environment? ")))
	 nil)
	(t
	 (message "Building System Environment...")
	 (sit-for 2)
	 (br-real-build-alists br-sys-search-dirs)
	 (if (interactive-p) (br-feature-build-htables))
	 (setq br-sys-paths-htable (hash-make br-paths-alist)
	       br-sys-parents-htable (hash-make br-parents-alist))
	 (run-hooks 'br-after-build-sys-hook)
	 (br-env-set-htables)
	 ;; Set prev-search-dirs so table rebuilds are not triggered.
	 (setq br-sys-prev-search-dirs br-sys-search-dirs)
	 (if (interactive-p) (br-env-save))
	 (message "Building System Environment...Done")
	 t)))

(defun br-class-in-table-p (class-name)
  "Return t iff CLASS-NAME is referenced within the current Environment."
  (interactive (list (br-complete-class-name)))
  (if class-name (hash-key-p class-name (br-get-parents-htable))))

;;; The OO-Browser depends on the name of this next function; don't change it.
(defun br-class-list-identity (class-list top-only-flag)
  class-list)

(defun br-class-path (class-name &optional insert)
  "Return full path, if any, to CLASS-NAME.
With optional prefix argument INSERT non-nil, insert path at point.
Only the first matching class is returned, so each CLASS-NAME should be
unique. Set `br-lib/sys-search-dirs' properly before use."
  (interactive (list (br-complete-class-name)))
  (setq class-name (if class-name (br-set-case class-name)))
  (let* ((class-path)
	 (class-htable (br-get-paths-htable)))
    (catch 'done
      (hash-map
       (function (lambda (val-key-cons)
		   (and (br-member class-name (car val-key-cons))
			(setq class-path (br-select-path val-key-cons nil))
			(throw 'done nil))))
       class-htable))
    (if (equal class-path br-null-path)
	(setq class-path nil))
    (and (interactive-p) (setq insert current-prefix-arg))
    (if (and insert class-path)
	(insert class-path)
      (if (interactive-p)
	  (message
	   (or class-path
	       (format
		"(OO-Browser):  No `%s' class found in `br-lib/sys-search-dirs'."
		class-name)))))
    class-path))

(defun br-find-class (&optional class-name view-only other-win)
  "Display file of class text matching CLASS-NAME in VIEW-ONLY mode if non-nil.
Return the line number of the start of the class displayed when successful, nil
otherwise.  Can also signal an error when called interactively."
  (interactive)
  (and (interactive-p) (setq view-only current-prefix-arg))
  (let ((class-path)
	(info (equal br-lang-prefix "info-"))
	(found)
	(err))
    (setq class-name
	  (or class-name (br-complete-class-name))
	  class-path (br-class-path class-name))
    (cond 
     (info (br-find-info-node class-path class-name (not view-only))
	   (setq found (br-line-number)))
     (class-path
      (if (file-readable-p class-path)
	  (progn (if view-only 
		     (funcall br-view-file-function class-path other-win)
		   (funcall br-edit-file-function class-path other-win)
		   ;; Handle case of already existing buffer in
		   ;; read only mode.
		   (and buffer-read-only
			(file-writable-p class-path)
			(progn (setq buffer-read-only nil)
			       ;; Force mode-line redisplay
			       (set-buffer-modified-p
				(buffer-modified-p)))))
		 (br-major-mode)
		 (let ((opoint (point))
		       (case-fold-search)
		       (start)
		       (pmin (point-min))
		       (pmax (point-max)))
		   (widen)
		   (goto-char (point-min))
		   (if br-narrow-view-to-class
		       ;; Display file narrowed to definition of
		       ;; `class-name'.
		       (if (br-to-class-definition class-name)
			   ;; Narrow display to this class
			   (progn (narrow-to-region
				   (progn (setq opoint
						(goto-char
						 (match-beginning 0)))
					  (br-back-over-comments)
					  (setq start (point))
					  (goto-char opoint)
					  start)
				   (progn (br-to-class-end)
					  (point)))
				  (goto-char opoint))
			 (goto-char opoint)
			 (narrow-to-region pmin pmax)
			 (setq err (format "(OO-Browser):  No `%s' in \"%s\""
					   class-name class-path)))
		     (if (br-to-class-definition class-name)
			 (progn (setq opoint (goto-char (match-beginning 0)))
				(br-back-over-comments)
				(goto-char opoint))
		       (goto-char opoint)
		       (narrow-to-region pmin pmax)
		       (setq err (format "(OO-Browser):  No `%s' in %s" class-name
					class-path)))))
		 (if err nil (setq found (br-line-number))))
	(setq err (format "(OO-Browser):  %s's source file, \"%s\", was not found or was unreadable"
			  class-name class-path))))
     ((interactive-p)
      (setq err (format "(OO-Browser):  No `%s' class defined in Environment"
			class-name))))
    (if err (error err))
    (if (interactive-p)
	(message "(OO-Browser):  `%s' class in \"%s\""
		 class-name (br-env-substitute-home class-path)))
    found))

(defun br-to-class-definition (class-name)
  "Search forward past the definition signature of CLASS-NAME within the current buffer.
Return non-nil iff definition is found.  Leave match patterns intact after the search."
  (let ((class-def (br-class-definition-regexp class-name))
	(found))
    (while (and (setq found (re-search-forward class-def nil t))
		(fboundp 'c-within-comment-p)
		(save-match-data (c-within-comment-p)))
      (setq found nil))
    found))

(defun br-back-over-comments ()
  "Skip back over any preceding comments stopping if the current line would leave the window."
  (let ((opoint (point)))
    ;; Language-specific.
    (br-to-comments-begin)
    ;; The following elaborate conditional is true if moving back past the
    ;; comments has pushed the original display line outside of the window
    ;; bounds.  The simpler check of (window-end) is not used because
    ;; redisplay has not yet been run and thus (window-end) does not yet
    ;; recorded the updated value.  It is better to err on the high
    ;; side when subtracting lines from the (window-height) here to ensure
    ;; that the original line is displayed.
    (if (> (count-lines (point) opoint) (- (window-height) 3))
	(progn (goto-char opoint)
	       ;; Make current line the last in the window in order to show as
	       ;; much commentary as possible without scrolling the current
	       ;; code line outside of the window.
	       (recenter -1))
      (recenter 0))))

(defun br-line-number ()
  "Return the present absolute line number within the current buffer counting from 1."
  (save-excursion
    (beginning-of-line)
    (1+ (count-lines 1 (point)))))

(defun br-display-code (start)
  "Goto START, skip back past preceding comments and then display."
  (goto-char start)
  (skip-chars-forward " \t\n\r")
  (setq start (point))
  ;; If the definition line is at the end of a list,
  ;; e.g. following a closing brace, ensure that the
  ;; beginning of the expression is visible.
  (beginning-of-line)
  (if (looking-at "[ \t]*\\s\)")
      (progn (goto-char (match-end 0))
	     (condition-case ()
		 (progn (backward-list 1)
			(beginning-of-line))
	       (error nil))))
  (br-back-over-comments)
  (goto-char start)
  t)

(defun br-major-mode ()
  "Invoke language-specific major mode on current buffer if not already set."
  (or (eq major-mode (symbol-function 'br-lang-mode))
      ;; directory
      (not buffer-file-name)
      (br-lang-mode)))

(defun br-class-category-p (name)
  "Return (category) if NAME contains a class category."
  (and (string-equal br-lang-prefix "objc-")
       (string-match "\([^\)]+\)" name)
       (substring name (match-beginning 0) (match-end 0))))

(defalias 'br-protocol-support-p 'br-interface-support-p)
(defun br-interface-support-p ()
  "Return t if the present OO-Browser Environment language contains interface (protocol) browsing support."
  (if (br-member br-lang-prefix '("java-" "objc-")) t))

(defalias 'br-protocol-p 'br-interface-p)
(defun br-interface-p (class-name)
  "Return CLASS-NAME if it is an interface or protocol which specifies method interfaces to which classes conform, else nil.
In Java, these are called interfaces.  In Objective-C, they are called protocols."
  (and (br-interface-support-p)
       (eq (aref class-name 0) ?\<)
       class-name))

(defun br-default-class-p (class-name)
  "Return CLASS-NAME if it is a default class, one generated by the OO-Browser, else nil."
  (if (eq (aref class-name 0) ?\[) class-name))

(defun br-concrete-class-p (class-name)
  "Return CLASS-NAME if it is not an interface or protocol, which generally means that all methods are implemented, else nil."
  (if (not (br-interface-p class-name)) class-name))

(defun br-scan-mode ()
  "Invoke language-specific major mode for current buffer without running its hooks.
This is used when scanning source files to build Environments."
  (let ((mode-hook-sym
	  (intern-soft (concat (symbol-name (symbol-function 'br-lang-mode))
			       "-hook"))))
    (if mode-hook-sym
	(eval (` (let ((, mode-hook-sym)) (br-lang-mode))))
      (br-lang-mode))))

(defun br-show-children (class-name)
  "Return children of CLASS-NAME from current Environment."
  (interactive (list (br-complete-class-name t)))
  (and class-name
       (br-get-children class-name)))

(defun br-show-parents (class-name)
  "Return parents of CLASS-NAME from Environment or scan of current buffer's source."
  (interactive (list (br-complete-class-name t)))
  (if class-name
      (if (br-class-in-table-p class-name)
	  (br-get-parents class-name)
	(if (and buffer-file-name (file-readable-p buffer-file-name))
	    (let ((br-view-file-function 'br-insert-file-contents))
	      (br-get-parents-from-source buffer-file-name class-name))))))

(defun br-undefined-classes ()
  "Return a list of the classes referenced but not defined within the current Environment."
  (let ((classes (hash-get br-null-path (br-get-paths-htable))))
    (delq nil (mapcar (function (lambda (class)
				  ;; Remove default classes
				  (if (not (eq (aref class 0) ?\[))
				      class)))
		      classes))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun br-add-to-paths-htable (class-name paths-key htable)
  "Add CLASS-NAME under PATHS-KEY in paths lookup HTABLE."
  (let ((other-classes (hash-get paths-key htable)))
    (if (and other-classes (br-member class-name other-classes))
	nil
      (hash-add (cons class-name other-classes) paths-key htable))))

(defun br-build-lib-parents-htable ()
  (interactive)
  (if (not br-lib-search-dirs)
      nil
    (message "Building Library parents...")
    (sit-for 2)
    (if br-lib-paths-htable
	(setq br-lib-parents-htable
	      (hash-make
	       (br-real-build-parents-alist br-lib-paths-htable)))
      (br-real-build-alists br-lib-search-dirs)
      (br-feature-build-htables)
      (setq br-lib-parents-htable (hash-make br-parents-alist)))
    (if (interactive-p) (br-env-save))
    (message "Building Library parents...Done")))

(defun br-build-lib-paths-htable ()
  (interactive)
  (if (not br-lib-search-dirs)
      nil
    (message "Building Library paths...")
    (sit-for 2)
    (br-real-build-alists br-lib-search-dirs)
    (br-feature-build-htables)
    (setq br-lib-paths-htable (hash-make br-paths-alist))
    (if (interactive-p) (br-env-save))
    (message "Building Library paths...Done")))

(defun br-build-sys-parents-htable ()
  (interactive)
  (if (not br-sys-search-dirs)
      nil
    (message "Building System parents...")
    (sit-for 2)
    (if br-sys-paths-htable
	(setq br-sys-parents-htable
	      (hash-make
	       (br-real-build-parents-alist br-sys-paths-htable)))
      (br-real-build-alists br-sys-search-dirs)
      (br-feature-build-htables)
      (setq br-sys-parents-htable
	    (hash-make br-parents-alist)))
    (if (interactive-p) (br-env-save))
    (message "Building System parents...Done")))

(defun br-build-sys-paths-htable ()
  (interactive)
  (if (not br-sys-search-dirs)
      nil
    (message "Building System paths...")
    (sit-for 2)
    (br-real-build-alists br-sys-search-dirs)
    (br-feature-build-htables)
    (setq br-sys-paths-htable (hash-make br-paths-alist))
    (if (interactive-p) (br-env-save))
    (message "Building System paths...Done")))

(defun br-build-children-htable ()
  (interactive)
  (setq br-children-htable (br-real-build-children-htable))
  (if (interactive-p) (br-env-save)))

(defun br-build-parents-htable ()
  (interactive)
  (br-build-sys-parents-htable)
  (br-build-lib-parents-htable)
  ;; Make System entries override Library entries which they duplicate, since
  ;; this is generally more desireable than merging the two.
  (br-merge-parents-htables)
  (if (interactive-p) (br-env-save)))

(defun br-merge-parents-htables ()
  (let ((hash-merge-values-function 'hash-merge-first-value))
    (setq br-parents-htable (hash-merge br-sys-parents-htable
					br-lib-parents-htable))))

(defun br-build-paths-htable ()
  (interactive)
  (br-build-sys-paths-htable)
  (br-build-lib-paths-htable)
  (br-merge-paths-htables)
  (if (interactive-p) (br-env-save)))

(defun br-merge-paths-htables ()
  (setq br-paths-htable (hash-merge br-sys-paths-htable br-lib-paths-htable))
  ;;
  ;; We may have merged two tables where a single class-name was referenced
  ;; in one and defined in the other which means it will have both a path
  ;; entry and a br-null-path entry; remove the latter.
  (let ((null-path-classes (hash-get br-null-path br-paths-htable))
	(null-lib-classes (sort (hash-get br-null-path br-lib-paths-htable)
				'string-lessp))
	(null-sys-default-classes
	 (delq nil
	       (mapcar (function (lambda (class)
				   (if (br-default-class-p class) class)))
		       (hash-get br-null-path br-sys-paths-htable))))
	(null-sys-referenced-classes
	 ;; Eliminates default classes; need this for System classes only
	 ;; since default classes are not added to the Library hash tables.
	 (sort 
	  (delq nil
		(mapcar (function (lambda (class)
				    (if (br-default-class-p class) nil class)))
			(hash-get br-null-path br-sys-paths-htable)))
	  'string-lessp)))
    (unwind-protect
	(progn (hash-delete br-null-path br-paths-htable)
	       (setq null-path-classes
		     (delq nil
			   (mapcar
			    (function
			     (lambda (class)
			       (if (or (br-default-class-p class)
				       (not (br-class-path class)))
				   class
				 ;; Class path exists, but we may need to
				 ;; remove a null entry from the Library or
				 ;; System table if it was referenced but not
				 ;; defined in one of these.
				 (setq null-lib-classes
				       (br-delete-sorted-strings
					class null-lib-classes))
				 (setq null-sys-referenced-classes
				       (br-delete-sorted-strings
					class null-sys-referenced-classes))
				 nil)))
			    null-path-classes))))
      (if null-path-classes
	  (hash-add null-path-classes br-null-path br-paths-htable)
	(hash-delete br-null-path br-paths-htable))
      (if null-lib-classes
	  (hash-add null-lib-classes br-null-path br-lib-paths-htable)
	(hash-delete br-null-path br-lib-paths-htable))
      (let ((null-sys-classes (nconc null-sys-default-classes
				     null-sys-referenced-classes)))
	(if null-sys-classes
	    (hash-add null-sys-classes br-null-path br-sys-paths-htable)
	  (hash-delete br-null-path br-sys-paths-htable))))))

(defun br-class-defined-p (class)
  "Return path for CLASS if defined in current Environment.
Otherwise, display error and return nil."
  (or (br-class-path class)
      (progn
	(beep)
	(message
	 (if (br-class-in-table-p class)
	     (format "(OO-Browser):  Class `%s' referenced but not defined in Environment."
		     class)
	   (format "(OO-Browser):  Class `%s' not defined in Environment."
		   class)))
	nil)))

(defun br-check-for-class (cl &optional other-win)
  "Try to display class CL.
Display message and return nil if unsucessful."
  (if (br-class-in-table-p cl)
      (or (br-find-class cl nil other-win)
	  (progn
	    (beep)
	    (message
	     (format "(OO-Browser):  Class `%s' referenced but not defined in Environment."
		     cl))
	    t))))

(defun br-get-parents (class-name)
  "Return list of parents of CLASS-NAME from parent lookup table.
Those from which CLASS-NAME directly inherits."
  (setq class-name (if class-name (br-set-case class-name)))
  (br-set-of-strings (hash-get class-name (br-get-parents-htable))))

(defun br-get-children (class-name)
  "Return list of children of CLASS-NAME from child lookup table.
Those which directly inherit from CLASS-NAME."
  (setq class-name (if class-name (br-set-case class-name)))
  (br-set-of-strings (hash-get class-name (br-get-children-htable))))

(defun br-get-children-from-parents-htable (class-name)
  "Returns list of children of CLASS-NAME.
Those that directly inherit from CLASS-NAME.  Uses parent lookup table to
compute children."
  (setq class-name (and class-name (br-set-case class-name)))
  (delq nil (hash-map (function (lambda (cns)
				  (if (and (consp cns)
					   (br-member class-name (car cns)))
				      (cdr cns))))
		      (br-get-parents-htable))))

(defun br-get-htable (htable-type)
  "Returns hash table corresponding to string, HTABLE-TYPE.
When necessary, loads the hash table from a file or builds it."
  (let* ((htable-symbol (intern-soft (concat "br-" htable-type "-htable")))
	 (htable-specific (if (string-match "sys\\|lib" htable-type)
			      (substring htable-type (match-beginning 0)
					 (match-end 0))))
	 changed-types non-matched-types)
    (if (equal htable-type "children")
	nil
      (if (and (or (not htable-specific) (equal htable-specific "lib"))
	       (or (null (symbol-value htable-symbol))
		   (not (equal br-lib-prev-search-dirs br-lib-search-dirs))))
	  (setq changed-types '("lib")))
      (if (and (or (not htable-specific) (equal htable-specific "sys"))
	       (or (null (symbol-value htable-symbol))
		   (not (equal br-sys-prev-search-dirs br-sys-search-dirs))))
	  (setq changed-types (cons "sys" changed-types))))
    (if (and (or br-lib-search-dirs br-sys-search-dirs)
	     (or changed-types (null (symbol-value htable-symbol)))
	     (not (boundp 'br-loaded)))
	;;
	;; Then need to load or rebuild htable.
	;;
	(progn (if (and br-env-file
			(file-exists-p br-env-file))
		   ;;
		   ;; Try to load from file.
		   ;;
		   (progn (setq non-matched-types
				(br-env-load-matching-htables changed-types))
			  (if non-matched-types
			      (setq changed-types
				    (delq nil (mapcar
					       (function
						(lambda (type)
						  (if (br-member type
								 changed-types)
						      type)))
					       non-matched-types)))
			    (and changed-types (br-env-set-htables t))
			    (setq changed-types nil)
			    (cond (htable-specific)
				  ((equal htable-type "children")
				   (progn (goto-char (point-min))
					  (setq br-children-htable
						(cdr (br-env-file-sym-val
						      "br-children-htable")))))
				  ((let ((suffix
					  (concat "-" htable-type "-htable"))
					 (hash-merge-values-function
					  'hash-merge-values))
					 ;; Make System entries override
					 ;; Library entries which they
					 ;; duplicate, if this is the parents
					 ;; htable.
				     (if (equal htable-type "parents")
					 (setq hash-merge-values-function
					       'hash-merge-first-value))
				     (set htable-symbol
					  (hash-merge
					   (symbol-value
					    (intern-soft
					     (concat "br-sys" suffix)))
					   (symbol-value
					    (intern-soft
					     (concat
					      "br-lib" suffix)))
					   ))))))))
	       ;; Rebuild any lists that need to be changed.
	       (mapcar
		(function
		 (lambda (type-str)
		   (let ((suffix (concat "-" htable-type "-htable")))
		     (funcall (intern-soft
			       (if (string-match "sys\\|lib" htable-type)
				   (concat "br-build" suffix)
				 (concat "br-build-" type-str suffix))))
		     (and htable-specific
			  ;; Make System entries override Library entries
			  ;; which they duplicate, if this is the parents
			  ;; htable.
			  (let ((hash-merge-values-function
				 'hash-merge-values))
			    (if (equal htable-type "parents")
				(setq hash-merge-values-function
				      'hash-merge-first-value))
			    (set htable-symbol
				 (hash-merge (symbol-value
					      (intern-soft
					       (concat "br-sys" suffix)))
					     (symbol-value
					      (intern-soft
					       (concat "br-lib" suffix)))
					     )))))))
		changed-types)
	       (if (and changed-types br-env-file)
		   (br-env-save))
	       (let ((buf (get-file-buffer br-env-file)))
		 (and buf (kill-buffer buf)))))
    ;; Return non-nil hash table.
    (if (null (symbol-value htable-symbol))
	(set htable-symbol (hash-make 0))
      (symbol-value htable-symbol))))

(defun br-get-top-class-list (htable-type-str)
    "Returns unordered list of top-level classes.
Those that do not explicitly inherit from any other classes.  Obtains classes
from list denoted by HTABLE-TYPE-STR whose values may be:
\"parents\", \"sys-parents\", or \"lib-parents\"."
    (delq nil (hash-map (function
			  (lambda (cns)
			    (if (null (car cns)) (cdr cns))))
			(br-get-htable htable-type-str))))

(defun br-get-top-classes ()
  "Returns lexicographically ordered list of top-level classes.
Those that do not explicitly inherit from any other classes."
  (br-get-top-class-list "parents"))

(defun br-get-lib-top-classes ()
  "Returns lexicographically ordered list of top-level Library classes.
Those that do not explicitly inherit from any other classes."
  (br-get-top-class-list "lib-parents"))

(defun br-get-sys-top-classes ()
  "Returns lexicographically ordered list of top-level System classes.
Those that do not explicitly inherit from any other classes."
  (br-get-top-class-list "sys-parents"))

(defun br-has-children-p (class-name)
  "Return non-nil iff CLASS-NAME has at least one child.
That is a class that directly inherits from CLASS-NAME."
  (setq class-name (and class-name (br-set-case class-name)))
  (hash-get class-name (br-get-children-htable)))

(defun br-has-parents-p (class-name)
  "Return non-nil iff CLASS-NAME has at least one parent.
That is a class which is a direct ancestor of CLASS-NAME."
  (setq class-name (and class-name (br-set-case class-name)))
  (hash-get class-name (br-get-parents-htable)))

(defun br-get-process-group (group max)
  "Return list of all active processes in GROUP (a string).
MAX is max number of processes to check for."
  (let ((i 0)
	(proc-list))
    (while (<= i max)
      (setq i (1+ i)
	    proc-list (cons (get-process (concat group (int-to-string i)))
			    proc-list)))
    (delq nil proc-list)))


(defun br-kill-process-group (group max group-descrip)
  "Optionally question user, then kill all subprocesses in named GROUP.
Processes are numbered one to MAX, some of which may have been killed already.
User is prompted with a string containing GROUP-DESCRIP, only if non-nil.
Return list of processes killed."
  (let ((proc-list (br-get-process-group group max)))
    (if proc-list
	(if (or (null group-descrip)
		(y-or-n-p (concat "Terminate all " group-descrip "? ")))
	    (prog1 (mapcar 'delete-process proc-list)
	      (message ""))))))

(defun br-real-add-class (lib-table-p class-name class-path &optional replace)
  "Add or replace class and its features within the current Environment.
If LIB-TABLE-P is non-nil, add to Library Environment, otherwise add to
System Environment.  Add class CLASS-NAME located in CLASS-PATH to
Environment.  If CLASS-PATH is nil, use current buffer file as CLASS-PATH.
Optional REPLACE non-nil means replace already existing class.  Does not
update children lookup table."
  (or class-path (setq class-path buffer-file-name))
  (let ((par-list)
	(paths-key class-path)
	(func)
	(class class-name))
    (if replace
	(setq func 'hash-replace
	      class-name (br-first-match
			  (concat "^" (regexp-quote class-name) "$")
			  (hash-get paths-key
				    (if lib-table-p 
					(br-get-htable "lib-paths")
				      (br-get-htable "sys-paths"))))
	      par-list
	      (and (stringp class-path) (file-readable-p class-path)
		   (let ((br-view-file-function 'br-insert-file-contents))
		     (br-get-parents-from-source class-path class-name))))
      (setq func 'hash-add))
    ;; Signal error if class-name is invalid.
    (if (null class-name)
	(if replace
	    (error "(br-real-add-class): `%s' not found in %s classes, so cannot replace it."
		   class (if lib-table-p "Library" "System"))
	  (error
	   "(br-real-add-class): Attempt to add null class to %s classes."
	   (if lib-table-p "Library" "System"))))
    ;;
    (mapcar
     (function
      (lambda (type)
	(let ((par-htable (br-get-htable (concat type "parents")))
	      (path-htable (br-get-htable (concat type "paths"))))
	  (funcall func par-list class-name par-htable)
	  (br-add-to-paths-htable class-name paths-key path-htable))))
     (list (if lib-table-p "lib-" "sys-") ""))
    (and (stringp class-path) (file-readable-p class-path)
	 (let ((br-view-file-function 'br-insert-file-contents))
	   (br-get-classes-from-source class-path)))))

(defun br-real-delete-class (class-name)
  "Delete class CLASS-NAME from current Environment.
No error occurs if the class is undefined in the Environment."
  (require 'set)
  (br-feature-tags-delete class-name)
  (let ((paths-key (br-class-path class-name))
	htable)
    (setq class-name
	  (br-first-match (concat "^" class-name "$")
			  (hash-get paths-key (br-get-paths-htable))))
    (if class-name
	(progn (mapcar
		 (function
		   (lambda (type)
		    (hash-delete class-name 
				 (br-get-htable (concat type "parents")))
		    (setq htable (br-get-htable (concat type "paths")))
		    (if (hash-key-p paths-key htable)
			(hash-replace
			 (set:remove
			  class-name
			  (hash-get paths-key htable))
			 paths-key htable))))
		 '("lib-" "sys-" ""))
	       (hash-delete class-name (br-get-children-htable))
	       (if (hashp br-features-htable)
		   (hash-delete class-name br-features-htable))))))

(defun br-real-build-children-htable ()
  "Build and return Environment parent to child lookup table."
  (let* ((par-ht (br-get-parents-htable))
	 (htable (hash-make (hash-size par-ht)))
	 (child))
    (hash-map
      (function
	(lambda (par-child-cns)
	  (setq child (cdr par-child-cns))
	  (mapcar
	    (function
	      (lambda (parent)
		(hash-add
		  (cons child (hash-get parent htable))
		  parent htable)))
	    (car par-child-cns))))
      par-ht)
    (hash-map (function
		(lambda (children-parent-cns)
		  (hash-replace (sort (car children-parent-cns) 'string-lessp)
				(cdr children-parent-cns) htable)))
	      htable)
    htable))

(defun br-real-get-children (class-name)
  "Return list of child classes of CLASS-NAME listed in Environment parents htable."
  (delq nil (hash-map
	      (function
		(lambda (cns)
		  (if (and (consp cns)
			   (br-member class-name (car cns)))
		      (cdr cns))))
	      (br-get-parents-htable))))

(defun br-real-build-alists (search-dirs)
  "Use SEARCH-DIRS to build `br-paths-alist' and `br-parents-alist'."
  (setq br-paths-alist nil br-parents-alist nil)
  (br-feature-tags-init nil)
  ;; These locals are used as free variables in the `br-real-build-al'
  ;; function. We define them here to prevent repeated stack usage as
  ;; that function recurses.
  (let ((inhibit-local-variables nil)
	(enable-local-variables t)
	(files)
	;; Treat as though running in batch mode so that major-mode-specific
	;; messages (e.g. those in Python mode) may be suppressed as files
	;; are read in for scanning.
	(noninteractive t)
	;; These are used in the `br-search-directory' function
	;; called by `br-real-built-al'.
	(br-view-file-function 'br-insert-file-contents)
	classes parents paths-parents-cons)
    (br-real-build-al search-dirs nil
		      (if (string-equal br-lang-prefix "python-")
			  'python-search-directory
			'br-search-directory)))
  (setq br-paths-alist br-paths-alist)
  br-paths-alist)

(defvar br-paths-alist nil)
(defvar br-parents-alist nil)

(defun br-skip-dir-p (dir-name)
  "Returns non-nil iff DIR-NAME is matched by a member of `br-skip-dir-regexps'."
  (delq nil
	(mapcar (function
		 (lambda (dir-regexp)
		   (string-match dir-regexp
				 (file-name-nondirectory
				  (directory-file-name dir-name)))))
		br-skip-dir-regexps)))

(defun br-real-build-al (search-dirs subdirectories-flag search-dir-func)
  "Descend SEARCH-DIRS and build `br-paths-alist' and `br-parents-alist'.
SUBDIRECTORIES-FLAG is t when SEARCH-DIRS are subdirectories of the root
Environment search directories.  SEARCH-DIR-FUNC is the function which
processes each directory, generally `br-search-directory'.
Does not initialize `br-paths-alist' or `br-parents-alist' to nil."
  (mapcar 
   (function
    (lambda (dir)
      (if (or (null dir) (equal dir "")
	      (progn (setq dir (file-name-as-directory dir))
		     ;; Skip subdirectory symlinks but not root-level ones.
		     (and subdirectories-flag
			  (file-symlink-p (directory-file-name dir))))
	      (br-skip-dir-p dir))
	  nil
	(setq files (if (and (file-directory-p dir)
			     (file-readable-p dir))
			(directory-files dir t br-file-dir-regexp)))
	;; Extract all class/parent names in all source files in a
	;; particular directory.
	(if files
	    (progn (message "Scanning %s in %s ..."
			    (file-name-nondirectory
			     (directory-file-name dir))
			    (br-abbreviate-file-name
			     (or (file-name-directory
				  (directory-file-name dir))
				 "")))
		   (funcall search-dir-func dir files)
		   ;; Call same function on all the directories below
		   ;; this one.
		   (br-real-build-al
		    (mapcar (function (lambda (f)
					(if (file-directory-p f) f)))
			    files)
		    t search-dir-func))))))
   search-dirs))

(defun br-search-directory (dir files)
  (mapcar
   (function
    (lambda (f)
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
	(sit-for 1))))
   ;; List of files potentially containing classes.
   (delq nil
	 (mapcar
	  (function
	   (lambda (f)
	     (and (string-match br-src-file-regexp f)
		  (not (file-directory-p f))
		  f)))
	  files))))

(defun br-real-build-parents-alist (paths-htable)
  "Build and return `br-parents-alist' of (parent-list . class) elements built from PATHS-HTABLE.
Initializes `br-parents-alist' to nil."
  (let ((inhibit-local-variables nil)
	(enable-local-variables t)
	(br-view-file-function 'br-insert-file-contents)
	dir)
    (hash-map
     (function
      (lambda (classes-file-cons)
	(setq dir (cdr classes-file-cons))
	(mapcar
	 (function
	  (lambda (class-name)
	    (setq br-parents-alist
		  (cons (cons
			 (and (stringp dir)
			      (file-exists-p dir)
			      (sort 
			       (br-get-parents-from-source
				dir class-name)
			       'string-lessp))
			 class-name)
			br-parents-alist))))
	 (car classes-file-cons))))
     paths-htable))
  br-parents-alist)

(defun br-set-lang-env (func sym-list val)
  "Use FUNC to set each element in SYM-LIST.
If VAL is non-nil, set `br' element to the value of the current OO-Browser
language element with the same name, otherwise set it to a function that
when called signals an error that the function is undefined for this language."
  (let ((br) (lang))
    (mapcar (function
	     (lambda (nm)
	       (setq br   (intern (concat "br-" nm))
		     lang (intern-soft (concat br-lang-prefix nm)))
	       (if (and (or (null lang) (not (boundp lang)))
			val)
		   ;; Don't try to set an unbound language-specific variable.
		   nil
		 (funcall func br (if val
				      (symbol-value lang)
				    (or lang 'br-undefined-function))))))
	    sym-list)))

(defun br-undefined-function (&rest ignore)
  (interactive)
  (error "(OO-Browser):  That command is not supported for this language."))

(defun br-setup-functions ()
  "Initialize appropriate function pointers for the current browser language."
  (br-set-lang-env 'fset
		   '("class-definition-regexp" "class-list-filter"
		     "get-classes-from-source" "get-parents-from-source"
		     "insert-class-info" "insert-entry-info"
		     "set-case" "set-case-type"
		     "store-class-info" "store-entry-info"
		     "to-class-end" "to-comments-begin" "to-definition"
		     "select-path"

		     "feature-edit-declaration"
		     "feature-implementors"
		     "feature-locate-p" "feature-name-to-regexp"
		     "feature-normalize"
		     "feature-signature-to-name"
		     "feature-signature-to-regexp"
		     "feature-view-declaration" "list-categories"
		     "list-protocols" "view-friend")
		   nil))

(defun br-setup-constants (env-file)
  "Initialize appropriate constant values for the current browser language using ENV-FILE."
  ;; Initialize auxiliary Env file variables.
  (br-init env-file)
  ;; Clear language-dependent hooks.
  (setq br-after-build-lib-hook nil
	br-after-build-sys-hook nil)
  ;; Set language-specific constants.
  (br-set-lang-env 'set '("class-def-regexp" "env-file"
			  "identifier" "identifier-chars"
			  "src-file-regexp" "narrow-view-to-class"
			  "tag-fields-regexp" "type-tag-separator")
		   t)
  (if (not (eq br-env-name t))
      (br-set-lang-env 'set '("env-name") t)))

(defun br-find-info-node (filename node edit)
  "Show (FILENAME)NODE in current window.
If EDIT is non-nil, NODE is made editable."
  (if (string-match "-[1-9][0-9]*$" filename)
      (setq filename (substring filename 0 (match-beginning 0))) )
  (Info-find-node filename node t)
  (if edit (let ((Info-enable-edit t))
	     (Info-edit))))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar br-lib-search-dirs nil
  "List of directories below which library dirs and source files are found.
A library is a stable group of classes.  Value is language-specific.")
(defvar br-sys-search-dirs nil
  "List of directories below which system dirs and source files are found.
A system is a group of classes that are likely to change.  Value is
language-specific.")

(defvar br-lib-prev-search-dirs nil
  "Used to check if `br-lib-paths-htable' must be regenerated.
Value is language-specific.")
(defvar br-sys-prev-search-dirs nil
  "Used to check if `br-sys-paths-htable' must be regenerated.
Value is language-specific.")

(defun br-pop-to-buffer (bufname &optional other-win read-only)
  "Display BUFNAME for editing, creating a new buffer if needed.
Optional OTHER-WIN means show in other window unless Hyperbole
is loaded in which case `hpath:display-buffer' determines where
to display the buffer.  Optional READ-ONLY means make the buffer
read-only."
  (interactive "BEdit or create buffer named: ")
  (funcall (cond ((br-in-browser)
		  (br-to-view-window)
		  (hpath:push-tag-mark)
		  'switch-to-buffer)
		 ((fboundp 'hpath:display-buffer)
		  'hpath:display-buffer)
		 (other-win 'switch-to-buffer-other-window)
		 (t 'switch-to-buffer))
	   (get-buffer-create bufname))
  (if read-only (setq buffer-read-only t)))

(defun br-find-file (filename &optional other-win read-only)
  "Edit file FILENAME.
Switch to a buffer visiting file FILENAME, creating one if none already
exists.  Optional OTHER-WIN means show in other window unless Hyperbole is
loaded in which case `hpath:display-buffer' determines where to display the
file.  Optional READ-ONLY means make the buffer read-only."
  (interactive "FFind file: ")
  (funcall (cond ((br-in-browser)
		  (br-to-view-window)
		  (hpath:push-tag-mark)
		  'switch-to-buffer)
		 ((fboundp 'hpath:display-buffer)
		  'hpath:display-buffer)
		 (other-win 'switch-to-buffer-other-window)
		 (t 'switch-to-buffer))
	   (find-file-noselect filename))
  (if read-only (setq buffer-read-only t)))

(defun br-find-file-read-only (filename &optional other-win)
  "Display file FILENAME read-only.
Switch to a buffer visiting file FILENAME, creating one if none
already exists.  Optional OTHER-WIN means show in other window."
  (interactive "FFind file read-only: ")
  (br-find-file filename other-win t))

(defvar br-edit-file-function 'br-find-file
  "*Function to call to edit a class file within the browser.")
(defvar br-view-file-function
  (if (eq br-edit-file-function 'br-find-file)
      'br-find-file-read-only
    br-edit-file-function)
  "*Function to call to view a class file within the browser.")

(defvar br-find-file-noselect-function 'br-find-file-noselect
  "Function to call to load a browser file but not select it.
The function must return the buffer containing the file's contents.")

(defvar *br-tmp-buffer* " oo-browser-tmp"
  "Name of temporary buffer used by the OO-Browser for parsing source files.")

(defun br-find-file-noselect (filename)
  "Read in the file given by FILENAME without selecting it or running any `find-file-hooks'."
  (let (find-file-hooks)
    (find-file-noselect filename)))

(defun br-insert-file-contents (filename &optional unused)
  "Insert after point into a temporary buffer the contents of FILENAME and temporarily select the buffer.
Optional second arg UNUSED is necessary since when used as a setting for
`br-view-file-function' this may be sent two arguments.

Does not run any find-file or mode specific hooks.  Marks buffer read-only to
prevent any accidental editing.

Set `br-view-file-function' to this function when parsing OO-Browser source
files for fast loading of many files."
  (set-buffer (get-buffer-create *br-tmp-buffer*))
  ;; Don't bother saving anything for this temporary buffer
  (buffer-disable-undo (current-buffer))
  (setq buffer-auto-save-file-name nil
	buffer-read-only nil)
  (erase-buffer)
  (insert-file-contents filename)
  (br-scan-mode)
  (setq buffer-read-only t))

(defvar br-lang-prefix nil
 "Prefix string that starts language-specific symbol names.")

(defvar br-children-htable nil
  "Htable whose elements are of the form: (LIST-OF-CHILD-CLASSES . CLASS-NAME).
Used to traverse class inheritance graph.  `br-build-children-htable' builds
this list.  Value is language-specific.")
(defvar br-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Used to traverse class inheritance graph.  `br-build-parents-htable' builds
this list.  Value is language-specific.")
(defvar br-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . DIRECTORY).
DIRECTORY gives the location of classes found in LIST-OF-CLASS-NAMES.
`br-build-paths-htable' builds this list.  Value is language-specific.")

(defvar br-features-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-FEATURES . CLASS).
`br-feature-build-htables' builds this htable.  Value is language-specific.")
(defvar br-feature-paths-htable nil
  "Htable whose elements are of the form: (PATHNAME . PATHNAME-NUMBER).
PATHNAME-NUMBER is an index stored in `br-features-htable' used to look up
the file of definition for individual class features.
`br-feature-build-htables' builds this htable.  Value is language-specific.")

(defvar br-lib-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from stable software libraries are used to build the list.
Value is language-specific.")
(defvar br-lib-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . DIRECTORY).
DIRECTORY gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from stable software libraries are used to build the list.
Value is language-specific.")

(defvar br-sys-parents-htable nil
  "Htable whose elements are of the form: (LIST-OF-PARENT-CLASSES . CLASS-NAME).
Only classes from systems that are likely to change are used to build the
list.  Value is language-specific.")
(defvar br-sys-paths-htable nil
  "Htable whose elements are of the form: (LIST-OF-CLASS-NAMES . DIRECTORY).
DIRECTORY gives the location of classes found in LIST-OF-CLASS-NAMES.
Only classes from systems that are likely to change are used to build the
list.  Value is language-specific.")

(defvar br-file-dir-regexp "\\`[^.~#]\\(.*[^.~#]\\)?\\'"
  "Regexp that matches only to files and directories that the OO-Browser should scan.
Others are ignored.")

(defvar br-src-file-regexp nil
  "Regular expression matching a unique part of source file names and no others.")

(defvar br-narrow-view-to-class nil
 "Non-nil means narrow buffer to just the matching class definition when displayed.
Don't set this, use the language specific variable instead.")

(provide 'br-lib)
