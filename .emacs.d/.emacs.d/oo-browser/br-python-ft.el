;;!emacs
;;
;; FILE:         br-python-ft.el
;; SUMMARY:      Python OO-Browser class and member functions.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     python, oop, tools
;;
;; AUTHOR:       Harri Pasanen / Bob Weiner
;;               based on Smalltalk and C++ OO-Browsers 
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:    5-Apr-96
;; LAST-MOD:     10-May-01 at 19:21:10 by Bob Weiner
;;
;; Copyright (C) 1996, 1997, 1998  BeOpen.com
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;    There may still be traces of C++ origin in this file.
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'br-c-ft)
(require 'br-python)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defconst python-feature-entry-regexp
  (concat br-feature-type-regexp " \\(.+\\.\\)?\\([^\t\n\r]*[^ \t\n\r]\\)")
  "Regexp matching a Python feature entry string from a browser listing buffer.")

(defvar python-import-dirs
  (if (stringp (getenv "PYTHONPATH"))
      (mapcar 'file-name-as-directory (split-string (getenv "PYTHONPATH") ":"))
    '("/usr/local/lib/python/"))
  "Ordered list of module directories searched by the python interpreter.
Each directory must end with a directory separator.")

(defconst python-type-tag-separator "@"
  "String that separates a tag's type from its normalized definition form.
This should be a single character which is unchanged when quoted for use as a
literal in a regular expression.")

(defconst python-tag-fields-regexp
  ;; The \\\\? below is necessary because we sometimes use this expression to
  ;; test against a string that has ben regexp-quoted and some of the
  ;; characters in br-feature-type-regexp will then be preceded by \\.
  (format "^\\([^%s \n]+\\)%s\\\\?\\(%s \\)\\([^%s\n]+\\)%s"
	  python-type-tag-separator python-type-tag-separator br-feature-type-regexp
	  python-type-tag-separator python-type-tag-separator)
 "Regexp matching the fields of a Python feature tag line.
Group 1 is the class of the feature.  Group 2 is the prefix preceding the
feature when displayed within a listing buffer.  Group 3 is the feature name.
The feature definition signature begins at the end of the regexp match,
i.e. (match-end 0), and goes to the end of the string or line.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun python-add-default-classes ()
  (if br-c-tags-flag
      (c-add-default-classes)
    ;; Add to categorize module functions
    (br-add-default-classes '("[function]")))
  (br-add-default-classes '("[global]" "[module]" "[package]")))

(defun python-feature-implementors (name)
  "Return unsorted list of Python feature tags which implement feature NAME."
  (nconc
   (python-feature-matches (concat "^" (regexp-quote name) "$"))
   (python-feature-matches (concat "\\." (regexp-quote name) "$"))))

(defun python-feature-signature-to-name (feature-sig-or-tag &optional with-class for-display)
  "Extracts the feature name from FEATURE-SIG-OR-TAG.
The feature's class name is dropped from feature-sig-or-tag unless optional
WITH-CLASS is non-nil.  If optional FOR-DISPLAY is non-nil, a feature type
character is prepended to the name for display in a browser listing."
  (if (br-feature-tag-p feature-sig-or-tag)
      (br-feature-tag-name feature-sig-or-tag with-class for-display)
    (let ((name))
      (cond
       ;; member
       ((string-match python-tag-fields-regexp feature-sig-or-tag)
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
       ;;
       ;; unknown
       (t;; Remove any trailing whitespace and add display prefix.
	(setq name (br-delete-space feature-sig-or-tag))
	(if for-display (concat "- " name) name))))))

(defun python-scan-features ()
  "Return reverse ordered list of current module's global attributes and function definitions.
Assume point is at the beginning of a widened buffer."
  (save-excursion
    (let ((case-fold-search)
	  (features) globals class name feat start)

      ;; Record globals
      (while (re-search-forward python-global-def nil t)
	(setq start (match-beginning 0))
	(if (python-within-string-p)
	    ;; ignore any feature found and skip to the end of the string
	    (re-search-forward python-multi-line-string-delimiter nil t)
	  ;; otherwise record the globals found
	  (setq globals (nreverse (python-scan-globals))
		feat (br-buffer-substring start (point)))
	  (setq features
		(nconc
		 (mapcar
		  (function (lambda (global)
			      (python-feature-normalize feat "[global]" global)))
		  globals)
		 features))))

      ;; Record non-member functions
      (goto-char (point-min))
      (while (re-search-forward python-routine-def nil t)
	(setq class "[function]"
	      name (br-buffer-substring
		    (match-beginning python-feature-name-grpn)
		    (match-end python-feature-name-grpn))
	      feat (python-feature-normalize
		    (br-buffer-substring (match-beginning 0) (match-end 0))
		    class name))
	(if (python-within-string-p)
	    ;; ignore any feature found and skip to the end of the string
	    (re-search-forward python-multi-line-string-delimiter nil t)
	  ;; otherwise record the function found
	  (setq features (cons feat features))))
      features)))

(defun python-to-definition (&optional other-win)
  "If point is on an import statement, look for the module file.
With OTHER-WIN non-nil, show it in another window."
  (interactive)
  (cond
   ((python-import-file other-win))
   (t	(beep)
	(message
	 "(OO-Browser):  Select an import statement to display its source.")
	nil)))

(defun python-insert-entry-info ()
  "Insert `python-docstring' into the current buffer at point."
  (interactive)
  (insert python-docstring))

(defalias 'python-insert-class-info 'python-insert-entry-info)

;; Optional arg below is solely for call compatibility when called with
;; an argument from br-store-class-info.  The argument is not used under
;; Python.
(defun python-store-entry-info (&optional entry)
  "Set `python-docstring' to the documentation for the listing entry at point and return it.
Return nil if no documentation is available."
 (setq python-docstring (python-lookup-docstring)))

(defalias 'python-store-class-info 'python-store-entry-info)

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun python-lookup-docstring ()
  "Look up and return a docstring for the browser listing entry at point or nil."
  (let ((entry-name)
	(entry-type)
	(file-name)
	(feature-tag)
	(docstring)
	(pydoc-interface-p (fboundp 'pydoc-commands)))

    (cond ((br-at-feature-p)
	   (setq feature-tag (br-feature-get-tag)
		 entry-name (br-feature-tag-name feature-tag)
		 file-name (br-feature-tag-path feature-tag))
	   ;; Entry-type may be: [package], [module], [global], [function]
	   ;; or a [method]
	   (setq entry-type (br-feature-tag-class feature-tag))
	   (if (not (br-member entry-type '("[package]" "[module]" "[global]"
					    "[function]")))
	       (setq entry-type "[method]")))

	  ((and (setq entry-name (br-find-class-name))
		(br-class-in-table-p entry-name))
	   (setq file-name (br-class-path entry-name))
	   ;; entry type may be a class or interface; consider them the
	   ;; same for documentation extraction purposes
	   (setq entry-type "[class]"))

	  (t (error "(OO-Browser):  Entry referenced but not defined in the Environment.")))

    (if (and file-name (not (string-equal entry-type "[global]")))
	(progn
	  (if pydoc-interface-p
	      (condition-case ()
		  (save-window-excursion
		    (require 'pydoc)
		    (cond ((string-equal entry-type "[package]")
			   (if (or (not (fboundp 'pydoc-package-list))
				   (br-member entry-name (pydoc-package-list)))
			       (progn (pydoc-packages entry-name)
				      (setq docstring (buffer-string)))))

			  ((string-equal entry-type "[module]")
			   (if (or (not (fboundp 'pydoc-module-list))
				   (br-member entry-name (pydoc-module-list)))
			       (progn (pydoc-modules entry-name)
				      (setq docstring (buffer-string)))))

			  ((br-member entry-type '("[function]" "[method]"))
			   (pydoc-help entry-name)
			   (setq docstring (buffer-string)))))
		(error
		 (setq docstring nil))))
	  (if (null docstring)
	      (setq docstring
		    (python-get-docstring-from-source
		     entry-name entry-type feature-tag file-name)))))

    docstring))

(defun python-get-docstring-from-source (entry-name entry-type feature-tag file-name)
  "Scan source for ENTRY-NAME's docstring using ENTRY-TYPE, FEATURE-TAG and FILE-NAME.
ENTRY-TYPE must a string, one of [package], [module], [function], [method] or 
\[class]."
  (let ((no-kill (get-file-buffer file-name))
	(docstring))
    (save-restriction
      (save-excursion
	(cond (no-kill
	       (set-buffer no-kill))
	      ((not (file-directory-p file-name))
	       (br-insert-file-contents file-name)))
	(widen)
	(goto-char (point-min))
	(cond ((string-equal entry-type "[class]")
	       (if (re-search-forward (python-class-definition-regexp
				       entry-name) nil t)
		   (progn
		     ;; Skip over any superclass list
		     (if (char-equal (preceding-char) ?\()
			 (progn (backward-char 1)
				(forward-list)))
		     (setq docstring (python-extract-docstring)))))

	      ((br-member entry-type '("[function]" "[method]"))
	       (if (python-feature-locate-p feature-tag)
		   ;; Skip past argument list
		   (if (> (skip-chars-forward "^\(") 0)
		       (progn
			 (forward-list)
			 (setq docstring (python-extract-docstring))))))

	      ((string-equal entry-type "[module]")
	       (setq docstring (python-extract-docstring)))

	      ((string-equal entry-type "[package]")
	       (setq file-name (expand-file-name "__init__.py" file-name)
		     no-kill (get-file-buffer file-name))
	       (if no-kill
		   (set-buffer no-kill)
		 (br-insert-file-contents file-name))
	       (setq docstring (python-extract-docstring))))))

    (if (not no-kill)
	(kill-buffer *br-tmp-buffer*))
    docstring))

(defun python-extract-docstring ()
  "Return the documentation string after point, or nil if none."
  (skip-chars-forward ": \t")
  (if (looking-at
       (concat python-empty-line "*[ \t]*" python-string-start))
      (let ((start (match-end 0))
	    (end-quote (br-buffer-substring (match-beginning 4) (match-end 4))))
	(goto-char start)
	(if (search-forward end-quote nil t)
	    (br-buffer-substring start (match-beginning 0))))))

(defconst python-string-start
  (concat 
   "\\("
   "'''"                ; triple single-quoted
   "\\|"		; or
   "\"\"\""	        ; triple double-quoted
   "\\|"		; or
   "'"		        ; single-quoted, not empty
   "\\|"		; or
   "\""	                ; double-quoted, not empty
   "\\)")
  "regexp matching python string literal starting quotes")

(defconst python-empty-line
  "\\(\\([ \t]*[\n\r]+\\)\\|\\([ \t]*#.*$\\)\\)"
  "Regexp matching an empty or python comment line.")

(defun python-within-comment-p ()
  "Return non-nil if point is within a Python comment."
  (save-excursion
    (and (re-search-backward "#\\|\n" nil t)
	 (not (looking-at "\n")))))

(defun python-within-string-p ()
  "Return non-nil if point is within a multi-line python string."
  (save-excursion
    (let ((count 0))
      (while (re-search-forward python-multi-line-string-delimiter nil t)
	(setq count (1+ count)))
      (= (% count 2) 1))))

(defun python-feature-map-tags (function regexp)
  "Apply FUNCTION to all current feature tags that match REGEXP and return a list of the results."
  (let ((identifier-chars (concat "[" python-identifier-chars ".]*"))
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

(defun python-feature-matches (regexp)
  "Return an unsorted list of feature tags whose names match in part or whole to REGEXP.
^ and $ characters may be used to match to the beginning and end of a feature name,
respectively."
  (python-feature-map-tags 'identity regexp))

(defun python-feature-normalize (feature class name)
  "Return a feature tag based on FEATURE, CLASS and NAME."
  (setq class (br-delete-space class))
  (setq name (if (equal class "[global]")
		 (concat "= " name)
	       (concat "- " name)))
  (let* ((len (length feature))
	 (normal-feature (make-string len ?\ ))
	 (n 0) (i 0)
	 (space-regexp "[ \t\n\r]+")
	 (original-syntax-table (syntax-table))
	 chr)
    (unwind-protect
	(progn
	  (set-syntax-table text-mode-syntax-table)
	  (while (< i len)
	    (setq chr (aref feature i)) 
	    (cond
	     ;; Convert sequences of space characters to a single space.
	     ;; GNU Emacs doesn't support optional syntax-table arg to
	     ;; `char-syntax'.
	     ((eq (char-syntax chr) ?\ )
	      (if (string-match space-regexp feature i)
		  (progn (setq i (match-end 0))
			 (if (not (and (> n 0)
				       (eq (aref normal-feature (1- n)) ?\ )))
			     (setq n (1+ n))))
		(setq i (1+ i)
		      n (1+ n))))
	     ;;
	     ;; Remove # comments
	     ((eq chr ?#)
	      (setq i (1+ i))
	      (while (and (< i len) (not (eq (aref feature i) ?\n)))
		(setq i (1+ i))))
	     (t ;; Normal character
	      (aset normal-feature n chr)
	      (setq i (1+ i)
		    n (1+ n)))))
	  (concat class python-type-tag-separator 
		  name python-type-tag-separator 
		  (br-delete-space (substring normal-feature 0 n))))
      (set-syntax-table original-syntax-table))))

(defun python-files-with-source (class)
  "Use CLASS to compute set of files that match to a Python source file regexp.
Return as a list."
  (let ((file (if class (br-class-path class) buffer-file-name)))
    (and file
	 (let* ((src-file-regexp (concat "^" (br-filename-head file)
					 python-code-file-regexp))
		(dir (file-name-directory file))
		(files (directory-files dir nil src-file-regexp)))
	   (mapcar (function (lambda (f) (expand-file-name f dir)))
		   files)))))

(defun python-find-ancestors-feature (class-list ftr-pat &optional other-win)
  "Scan ancestors of CLASS-LIST and show routine definition matching FTR-PAT."
  ;; If no class, search for a non-member function.
  (or class-list (setq class-list '(nil)))
  (br-feature-display class-list ftr-pat other-win))

(defun python-find-class-name ()
  "Return current word as a potential class name."
  (save-excursion
    (let* ((start)
	   (ignore "\]\[ \t\n\r\f\;,.\(\){}*&-")
	   (pat (concat "^" ignore)))
      (forward-char 1)
      (skip-chars-backward ignore)
      (skip-chars-backward pat)
      (setq start (point))
      (skip-chars-forward (concat pat ":"))
      (br-buffer-substring start (point)))))

(defun python-output-feature-tags (routine-file routine-tags-list)
  "Write Python ROUTINE-FILE's ROUTINE-TAGS-LIST into `br-feature-tags-file'.
Assume `br-feature-tags-init' has been called."
  (interactive)
  (save-excursion
    (br-feature-set-tags-buffer)
    (goto-char 1)
    ;; Delete any prior routine tags associated with routine-file
    (if (search-forward routine-file nil 'end)
	(progn (forward-line -1)
	       (let ((start (point)))
		 (search-forward "\^L" nil 'end 2)
		 (backward-char 1)
		 (delete-region start (point)))))
    (if routine-tags-list
	(progn (insert "\^L\n")
	       ;; Quote pathname to avoid read errors on MS OSes.
	       (prin1 routine-file (current-buffer))
	       (insert "\n")
	       (mapcar (function (lambda (tag) (insert tag "\n")))
		       routine-tags-list)))))

(defun python-find-module-name ()
  "Return current word as a potential module name."
  (save-excursion
    (let ((start))
      (forward-char 1)
      (skip-chars-backward python-identifier-chars)
      (setq start (point))
      (skip-chars-forward python-identifier-chars)
      (br-buffer-substring start (point)))))

(defun python-import-file (&optional other-win)
  "If point is on an import module line, display the module, method or function name at point.
With optional OTHER-WIN non-nil, display it in the other window.

Return non-nil iff point is on an import file line, even if a matching entry
is not found.  When found return the full pathname to the import entry,
otherwise return t.

Look for import files within `python-import-dirs' and any Environment
directory."
  (let ((opoint (point)))
    (beginning-of-line)
    (cond ((looking-at python-import-modules-regexp)
	   (if (< (match-end 0)
		  (max opoint (match-beginning python-import-name-grpn)))
	       ;; Have to avoid selecting anything within a # comment here.
	       (goto-char (match-beginning python-import-name-grpn))
	     (goto-char
	      (max opoint (match-beginning python-import-name-grpn))))
	   (let* ((import-name (python-find-module-name))
		  (path (python-import-pathname import-name)))
	     ;; If found, display file
	     (python-display-module import-name path other-win)
	     (or path t)))
	  ((looking-at python-import-functions-regexp)
	   (if (< (match-end 0)
		  (max opoint (match-beginning python-import-name-grpn)))
	       ;; Have to avoid selecting anything within a # comment here.
	       (goto-char (match-beginning python-import-name-grpn))
	     (goto-char
	      (max opoint (match-beginning python-import-name-grpn))))
	   (setq opoint (point))
	   (let* ((end-module-name (match-end python-import-name-grpn))
		  (module-name 
		   (br-buffer-substring
		    (match-beginning python-import-name-grpn)
		    (match-end python-import-name-grpn)))
		  (import-name (python-find-module-name))
		  (path (python-import-pathname module-name)))
	     ;; If found, display file
	     (if (python-display-module module-name path other-win)
		 (if (or (<= opoint end-module-name)
			 (equal import-name "import")
			 (equal import-name ""))
		     nil
		   (if (re-search-forward
			(concat "^[ \t]*\\(class\\|def\\|\\)"
				"\\(" (regexp-quote import-name) "\\)"
				"[^" python-identifier-chars "]")
			nil t)
		       (goto-char (match-beginning 2))
		     (beep)
		     (message "(OO-Browser):  Found module `%s' but not member `%s'."
			      module-name import-name))))
	     (or path t)))
	  (t (goto-char opoint)
	     nil))))

(defun python-display-module (module-name path other-win)
  "Display file associated with MODULE-NAME and PATH in OTHER-WIN (if non-nil).
Return t if file is displayed, nil otherwise."
  (if path
      (if (file-readable-p path)
	  (progn
	    (funcall br-edit-file-function path other-win)
	    (widen)
	    (goto-char (point-min))
	    (if (not (fboundp 'br-lang-mode))
		(python-mode-setup))
	    (br-major-mode)
	    t)
	(beep)
	(message "(OO-Browser):  Module `%s' is unreadable." path)
	nil)
    (beep)
    (message "(OO-Browser):  Cannot find module `%s'." module-name)
    nil))

(defun python-import-pathname (import-name)
  "Return the full pathname to a Python IMPORT-NAME or nil if none.
Look for import files within `python-import-dirs' and any Environment
directory."
  (if (not (stringp import-name))
      (error "(python-import-pathname): Invalid import name, `%s'" import-name))
  (if (string-match "\\.py\\'" import-name)
      (setq import-name (substring import-name 0 (match-beginning 0))))
  ;; Convert import-name a.b.c to pathname form, a/b/c.
  (setq import-name (hypb:replace-match-string
		     "\\." import-name
		     (file-name-as-directory "/")
		     t))
  (setq import-name (concat import-name ".py"))
  (let ((dir-list (append python-lib-search-dirs python-sys-search-dirs 
			  python-import-dirs))
	(found)
	path)
    (if buffer-file-name
	(setq dir-list (cons (file-name-directory buffer-file-name)
			     dir-list)))
    (while (and (not found) dir-list)
      (setq path (expand-file-name import-name (car dir-list)))
      (or (setq found (file-exists-p path))
	  (setq dir-list (cdr dir-list))))
    ;;
    ;; If not found in normal include dirs, check all Env paths also.
    ;;
    (if (not found)
	(let ((paths (delq nil (hash-map 'cdr br-paths-htable))))
	  (while (and (not found) paths)
	    (setq path (car paths))
	    (if (string-equal (file-name-nondirectory path) import-name)
		(setq found t paths nil)
	      (setq paths (cdr paths))))))
    (if found path)))

(defun python-scan-ancestors-feature (class-list ftr-pat &optional other-win)
  "Display routine definition derived from CLASS-LIST, matching FTR-PAT.
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
	      code-def-files (python-files-with-source class)
	      ftr-sig-regexp (funcall ftr-pat class))
	(while (and (setq file (car code-def-files))
		    (not (setq found-ftr
			       (br-feature-found-p file ftr-sig-regexp
						   nil other-win t))))
	  (setq code-def-files (cdr code-def-files)))
	(setq classes (if found-ftr nil (cdr classes))))
      (if found-ftr
	  (or class t)
	(python-scan-ancestors-feature
	 (apply 'append (mapcar (function (lambda (cl) (br-get-parents cl)))
				class-list))
	 ftr-pat)))))

(defun python-scan-features-in-class (class start end)
  "Return reverse ordered list of Python routine definitions within CLASS def.
START and END give buffer region to search."
  (setq class (br-delete-space class))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (let ((routines) rout name)
	;;
	;; Get member definitions
	;;
	(while (re-search-forward python-routine-def-in-class nil t)
	  (setq start (match-beginning 0)
		name (concat class "."
			     (br-buffer-substring
			      (match-beginning python-feature-name-grpn)
			      (match-end python-feature-name-grpn)))
		rout (python-feature-normalize
		      (br-buffer-substring (match-beginning 0) (match-end 0))
		      class name)
		routines (cons rout routines)))
	routines))))

(defun python-scan-globals ()
  "Return list of globals names from a single global statement.
Point must be after the 'global' keyword which begins the list of comma
separated identifiers."
  (let ((global-list) (again t)
	global)
    (while (and again (re-search-forward python-global-name nil t))
      (setq again (eq ?, (following-char))
	    global (br-buffer-substring (match-beginning 1)
					(match-end 1))
	    global-list (cons global global-list)))
    (nreverse global-list)))

(defun python-feature-locate-p (feature-tag &optional regexp-flag)
  "Leave point at the start of FEATURE-TAG's definition in the current buffer.
Assume caller has moved point to the beginning of the buffer or to the point
of desired search start.
Optional REGEXP-FLAG means FEATURE-TAG is a regular expression."
  (let ((case-fold-search) (start)
	(found t) feature-sig feature-regexp class)
    (if (br-feature-tag-p feature-tag)
	(setq feature-sig (br-feature-tag-signature feature-tag)
	      class (br-feature-tag-class feature-tag))
      (setq feature-sig feature-tag
	    class nil))
    (if regexp-flag
	(if (stringp feature-tag)
	    (setq feature-regexp feature-tag)
	  (error "(python-feature-locate-p): Not a regexp, %s" feature-tag)))
    ;;
    ;; First move to the proper class implementation if feature-tag does not
    ;; include a <class>:: part and this is not a [default-class], so that if
    ;; two classes in the same file have the same feature signature, we still
    ;; end up at the right one.
    (cond (class
	   (if (or (string-match "\\`\\[" class)
		   (and feature-sig (string-match "::" feature-sig)))
	       nil
	     (setq found (re-search-forward
			  (python-class-definition-regexp class nil)
			  nil t)
		   start (match-beginning 0))))
	  ((string-match python-tag-fields-regexp feature-sig)
	   (setq class (substring feature-sig
				  (match-beginning 1) (match-end 1))
		 feature-sig (substring feature-sig (match-end 0)))
	   (if (or (and regexp-flag
			(not (string-match "\\`\\\\\\[\\|::" feature-regexp)))
		   (not (or regexp-flag
			    (string-match "\\`\\[\\|::" feature-tag))))
	       (setq found (re-search-forward
			    (python-class-definition-regexp class regexp-flag)
			    nil t)
		     start (match-beginning 0)))))
    ;;
    ;; If class was searched for and not found, then skip down to code display.
    (if (not found)
	nil
      ;; Otherwise, look for feature expression.
      (setq found nil)
      (or regexp-flag (setq feature-regexp
			    (python-feature-signature-to-regexp feature-sig)))
      (while (and (re-search-forward feature-regexp nil t)
		  (setq start (match-beginning 0))
		  (not (setq found
			     (cond ((eq major-mode 'python-mode)
				    (cond ((python-within-comment-p)
					   nil)
					  ((python-within-string-p)
					   (re-search-forward
					    python-multi-line-string-delimiter nil t)
					   nil)
					  (t)))
				   ;; must be a C/C++ file
				   ((c-within-comment-p)
				    (search-forward "*/" nil t)
				    nil)
				   (t)))))))
    (if found (br-display-code start))))

(defun python-feature-name-to-regexp (name)
  "Converts routine NAME into a regular expression matching the routine's name tag."
  (setq name (python-feature-signature-to-regexp name))
  (aset name (1- (length name)) ?\()  ;; Match only to functions
  name)

(defun python-feature-signature-to-regexp (signature)
  "Given a Python SIGNATURE, return regexp used to match to its definition."
  (setq signature (regexp-quote signature))
  (let ((prefix-info
	 (if (string-match python-tag-fields-regexp signature)
	     (prog1 (substring signature (match-beginning 0) (match-end 0))
	       (setq signature (substring signature (match-end 0)))))))
    (let ((pat) (i 0) (c) (len (length signature)))
      (while (< i len)
	(setq c (aref signature i)
	      pat (cond ((eq c ? )
			 ;; Allow for possible single line comment
			 ;; following any whitespace, e.g. following
			 ;; each routine argument.
			 (concat pat "[ \t\n\r]*\\(#.*\\)?"))
			(t
			 (concat pat (char-to-string c))))
	      i (1+ i)))
      (setq pat (concat prefix-info pat)))))




;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar python-docstring ""
  "Documentation string for python class, method or function.")

(defconst python-code-file-regexp "\\.py\\'"
  "Regexp which matches a unique part of a Python source (non-header) file name and no others.")

(defconst python-import-modules-regexp
  (concat "^[ \t]*import[ \t]+" python-identifier "[^#\n\r]+")
  "Regexp which matches Python module import statements.
Grouping `python-import-name-grpn' matches the first import module name.")

(defconst python-import-functions-regexp
  (concat "^[ \t]*from[ \t]+" python-identifier "[ \t\n\r]+import[ \t]+[^#\n\r]+")
  "Regexp which matches Python function import statements.
Grouping `python-import-name-grpn' matches the import module name.")

(defconst python-global-def "^global[ \t]+"
  "Regexp which matches a global definition statement.
After a match to this expression, point is left after the 'global' keyword.")

(defconst python-routine-def
  (concat "^def[ \t]+" python-identifier "[ \t\n\r\\]*[^:]+")
  "Regexp which matches a global python function definition.
Grouping `python-feature-name-grpn' matches the function name.
After a match to this expression, point is left before the colon
terminating the signature line.")

(defconst python-routine-def-in-class
  (concat "^[ \t]+def[ \t]+" python-identifier "[ \t\n\r\\]*[^:]+")
  "Regexp which matches a python class method definition.
Grouping `python-feature-name-grpn' matches the function name.
After a match to this expression, point is left before the colon
terminating the signature line.")

(defconst python-feature-name-grpn 1
  "Feature name grouping from `python-routine-def' and `python-routine-def-in-class' matches.")

(defconst python-import-name-grpn 1
  "Module name regexp grouping for import statements.")

(defconst python-multi-line-string-delimiter "'''\\|\"\"\""
  "Regexp matching a Python multi-line string Start or end delimiter.")

(provide 'br-python-ft)
