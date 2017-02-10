;;!emacs
;;
;; FILE:         br-ftr.el
;; SUMMARY:      OO-Browser feature browsing support.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:    20-Aug-91 at 18:16:36
;; LAST-MOD:     10-May-01 at 19:08:32 by Bob Weiner
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(mapcar 'require '(br-c-ft hypb))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defconst br-attribute-type-regexp "[=&]"
  "Regular expression which matches the first non-whitespace character in an OO-Browser attribute listing.")

(defconst br-feature-type-regexp "[-+=&%>1/]"
  "Regular expression which matches the first non-whitespace character in an OO-Browser feature listing.")

(defconst br-routine-type-regexp "[-+>1/]"
  "Regular expression which matches the first non-whitespace character in an OO-Browser routine listing.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun br-edit-feature (class feature-name &optional other-win view-only)
  "Edit the definition of CLASS' FEATURE-NAME, optionally in some OTHER-WIN if non-nil.
With optional VIEW-ONLY non-nil, view the feature definition instead of editing it.
Return the pathname of the feature definition if found, else nil."
  (interactive
   (list nil (br-feature-complete 'must-match "Edit feature definition:")
	 nil nil))
  (let ((tag-and-file (br-feature-tag-and-file
		       (if (null class)
			   ;; Assume feature-name includes prepended class in
			   ;; proper format, e.g. when called interactively.
			   feature-name
			 (concat class "::" feature-name)))))
    (if tag-and-file (br-edit-feature-from-tag
		      (car tag-and-file) (cdr tag-and-file) other-win view-only))))

(defun br-edit-feature-from-tag (feature-tag feature-path &optional other-win view-only)
  "Edit feature for OO-Browser FEATURE-TAG of file FEATURE-PATH, optionally in OTHER-WIN if non-nil.
With optional VIEW-ONLY, view feature definition instead of editing it.
Return FEATURE-PATH if feature definition is found, else nil."
  (let ((err))
    (cond ((and feature-path (file-readable-p feature-path))
	   (cond ((br-feature-found-p feature-path feature-tag
				      nil other-win)
		  (if view-only
		      (setq buffer-read-only t)
		    ;; Handle case of already existing buffer in
		    ;; read only mode.
		    (and buffer-read-only
			 (file-writable-p feature-path)
			 (setq buffer-read-only nil)))
		  ;;
		  ;; Force mode-line redisplay
		  (set-buffer-modified-p (buffer-modified-p)))
		 ((interactive-p)
		  (setq err
			(format
			 "(OO-Browser):  No `%s' feature defined in Environment."
			 feature-tag)
			feature-path nil))))
	  ((interactive-p)
	   (setq err
		 (format
		  "(OO-Browser):  `%s' - src file not found or not readable, %s"
		  feature-tag feature-path)
		 feature-path nil))
	  ;; Feature not found.
	  (t (setq feature-path nil)))
    (if err (error err))
    feature-path))

(defun br-find-feature (&optional feature-entry view-only other-win)
  "Display feature definition for optional FEATURE-ENTRY in VIEW-ONLY mode if non-nil in OTHER-WIN if non-nil.
Return feature path if FEATURE-ENTRY is successfully displayed, nil
otherwise.  Can also signal an error when called interactively."
  (interactive)
  (and (interactive-p) (setq view-only current-prefix-arg))
  (let ((feature-path))
    (setq feature-entry
	  (br-feature-tag-and-file
	   (or feature-entry
	       (br-feature-complete 'must-match
				    (if view-only
					"View feature definition:"
				      "Edit feature definition:"))))
	  feature-path (cdr feature-entry)
	  feature-entry (car feature-entry))
    (br-edit-feature-from-tag feature-entry feature-path other-win view-only)))

(defun br-find-feature-entry ()
  "Return feature listing entry that point is within or nil.
Remove any leading whitespace but leave any prefix character."
  (if (= (point) (point-max)) (skip-chars-backward " \t\n\r"))
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (or (br-at-feature-p)
	    ;; Get current feature signature, if any.
	    (br-feature-get-tag))
	(let ((feature (br-buffer-substring
			(point)
			(progn (skip-chars-forward "^\t\n\r") (point)))))
	  (if (and (equal br-lang-prefix "objc-")
		   ;; Remove any trailing class from a category entry.
		   (string-match "([^\)]+)" feature))
	      (substring feature 0 (match-end 0))
	    feature)))))

(defun br-feature-entry ()
  "Return a listing entry as displayed in the buffer (sans leading whitespace)."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (br-buffer-substring
     (point) (progn (skip-chars-forward "^\t\n\r") (point)))))

(defun br-feature-ancestor-implementors (class-name feature-name method-flag)
  "Display an *Implementors* buffer with ancestor implementor listings matching CLASS-NAME and FEATURE-NAME.
The feature is a method if METHOD-FLAG is non-nil, otherwise, it is an attribute.
Return the number of implementors found."
  (br-feature-relation-implementors class-name feature-name
				    'br-feature-insert-ancestor-implementors
				    method-flag))

(defun br-feature-descendant-implementors (class-name feature-name method-flag)
  "Display an *Implementors* buffer with descendant implementor listings matching CLASS-NAME and FEATURE-NAME.
The feature is a method if METHOD-FLAG is non-nil, otherwise, it is an attribute.
Return the number of implementors found."
  (br-feature-relation-implementors class-name feature-name
				    'br-feature-insert-descendant-implementors
				    method-flag))

(defun br-feature-complete (&optional must-match prompt)
  "Interactively complete feature entry if possible, and return it.
Optional MUST-MATCH means must match a completion table entry.
Optional PROMPT is the initial prompt string for the user."
  (interactive)
  (let ((default (br-feature-default))
        (completion-ignore-case t)
	completions
	ftr-entry)
    ;; Prompt with possible completions of ftr-entry.
    (setq prompt (or prompt "Feature entry:")
	  completions (br-element-completions)
	  ftr-entry
	  (if completions
	      (completing-read
		(format "%s (default %s) " prompt default)
		completions nil must-match)
	    (read-string
	      (format "%s (default %s) " prompt default))))
    (if (equal ftr-entry "") default ftr-entry)))

(defun br-default-class-completions ()
  "Return completion alist of the names of all default class instances."
  (cond ((not (and br-env-file (file-exists-p br-env-file)
		   (file-readable-p br-env-file)))
	 nil)
	((and br-default-class-tags-completions
	      (eq
	       (car (cdr br-default-class-tags-completions)) ;; tags last mod time
	       (apply '+ (nth 5 (file-attributes br-env-file))))
	      (equal br-env-file (car br-default-class-tags-completions)))
	 (car (cdr (cdr br-default-class-tags-completions))))
	(t
	 (let ((elt-list)
	       (elt-alist)
	       (default-classes
		 (delq nil (mapcar 'br-default-class-p (br-all-classes)))))
	   (setq elt-list
		 (apply 'nconc
			(mapcar (function
				 (lambda (class)
				   (br-feature-map-class-tags
				    (function (lambda (tag)
						(br-feature-tag-name tag nil nil)))
				    class)))
				default-classes)))
	   (setq elt-list
		 (br-set-of-strings
		  (br-feature-tag-sort-list
		   (nconc elt-list
			  (delq nil
				(mapcar (function
					 (lambda (class)
					   (if (br-default-class-p class)
					       nil class)))
					(br-all-classes))))))
		 elt-alist (mapcar 'list elt-list)
		 br-default-class-tags-completions 
		 (list br-env-file
		       ;; tags last mod time
		       (apply '+ (nth 5 (file-attributes br-env-file)))
		       elt-alist))
	   elt-alist))))

(defun br-element-completions ()
  "Return completion alist of all current Environment elements."
  (cond ((not (and br-env-file (file-exists-p br-env-file)
		   (file-readable-p br-env-file)))
	 nil)
	((and br-element-tags-completions
	      (eq
	       (car (cdr br-element-tags-completions)) ;; tags last mod time
	       (apply '+ (nth 5 (file-attributes br-env-file))))
	      (equal br-env-file (car br-element-tags-completions)))
	 (car (cdr (cdr br-element-tags-completions))))
	(t
	 (message "(OO-Browser):  Computing element completions...")
	 (let ((elt-list (br-feature-map-all-tags
			  (function (lambda (tag)
				      (br-feature-tag-name tag t nil)))))
	       (elt-alist))
	   (setq elt-list (br-set-of-strings
			   (br-feature-tag-sort-list elt-list))
		 elt-alist (mapcar 'list elt-list)
		 br-element-tags-completions 
		 (list br-env-file
		       ;; tags last mod time
		       (apply '+ (nth 5 (file-attributes br-env-file)))
		       elt-alist))
	   (message "(OO-Browser):  Computing element completions...Done")
	   elt-alist))))

(defun br-feature-completions ()
  "Return completion alist of all current Environment features.
This excludes default class elements."
  (cond ((not (and br-env-file (file-exists-p br-env-file)
		   (file-readable-p br-env-file)))
	 nil)
	((and br-feature-tags-completions
	      (eq
	       (car (cdr br-feature-tags-completions)) ;; tags last mod time
	       (apply '+ (nth 5 (file-attributes br-env-file))))
	      (equal br-env-file (car br-feature-tags-completions)))
	 (car (cdr (cdr br-feature-tags-completions))))
	(t
	 (let ((ftr-alist)
	       (ftr-list
		(apply 'nconc
			(mapcar (function
				 (lambda (class)
				   (br-feature-map-class-tags
				    (function (lambda (tag)
						(br-feature-tag-name tag nil nil)))
				    class)))
				;; All classes except default classes.
				(delq nil
				      (mapcar (function
					       (lambda (class)
						 (if (br-default-class-p class)
						     nil class)))
					      (br-all-classes)))))))
	   (setq ftr-list (br-set-of-strings
			   (br-feature-tag-sort-list ftr-list))
		 ftr-alist (mapcar 'list ftr-list)
		 br-feature-tags-completions
		 (list br-env-file
		       ;; tags last mod time
		       (apply '+ (nth 5 (file-attributes br-env-file)))
		       ftr-alist))
	   ftr-alist))))

(defun br-feature-default ()
  "Return a best guess default for the feature or class name at point.
Try to return it in class::feature format."
  (cond ((and (br-browser-buffer-p) (br-listing-window-p))
         (let ((ftr-tag (br-feature-get-tag)))
           (if ftr-tag
               (br-feature-tag-name ftr-tag t)
             ;; assume is a class entry
             (br-find-class-name))))
        ((and (equal br-lang-prefix "c++-")
              (eq major-mode (symbol-function 'br-lang-mode)))
	 (let ((member-name) (class-name) member-elts)
	   (cond ((save-excursion (c++-feature-def-p))
		  (setq member-name
			(br-feature-signature-to-name
			 (br-buffer-substring (match-beginning 0) (match-end 0))
			 t)))
		 ((save-excursion (c++-skip-to-statement) (c++-feature-decl))
		  (setq member-name
			(br-feature-signature-to-name
			 (br-buffer-substring (match-beginning 0) (match-end 0))
			 t)))
		 (t (setq member-elts (c++-feature-at-reference-p)
			  member-name (nth 4 member-elts)
			  class-name (nth 2 member-elts))))
	   (if (or class-name
		   (and member-name (string-match "::" member-name)))
	       nil
	     ;; We know member-name is actually a type if it contains a
	     ;; <template> expression.
	     (if (and member-name (string-match "\<" member-name))
		 (setq member-name
		       (c++-normalize-template-arguments
			member-name))
	       (setq class-name (c++-feature-class-name))))
	   (cond ((and class-name member-name)
		  (concat class-name "::" member-name))
		 (member-name)
		 (class-name)
		 ((fboundp 'smart-c++-at-tag-p)
		  (smart-c++-at-tag-p))
		 ((fboundp 'find-tag-default)
		  (find-tag-default)))))
        ((eq major-mode (symbol-function 'br-lang-mode))
         ;; In a code buffer
         (let ((lang-tag-function
                (intern-soft (concat "smart-" br-lang-prefix "at-tag-p"))))
           (cond ((and lang-tag-function (fboundp lang-tag-function))
                  (funcall lang-tag-function))
                 ((fboundp 'find-tag-default)
                  (find-tag-default)))))
        (t (if (fboundp 'find-tag-default) (find-tag-default)))))

(defun br-feature-delete-c-comments (feature)
  "Convert multiple whitespace characters to single spaces and remove C/C++-style comments from FEATURE (a string) and return a new result string."
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
	     ;; Remove comments
	     ((and (< (setq i (1+ i)) len)
		   (eq chr ?/)
		   (cond
		    ((eq (aref feature i) ?/)
		     ;; Remove // style comments
		     (setq i (1+ i))
		     (while (and (< i len) (not (eq (aref feature i) ?\n)))
		       (setq i (1+ i)))
		     t)
		    ((eq (aref feature i) ?*)
		     ;; Remove C-style comments
		     (setq i (1+ i))
		     (while (and (< (1+ i) len)
				 (not (and (eq (aref feature i) ?*)
					   (eq (aref feature (1+ i)) ?/)
					   (setq i (+ i 2)))))
		       (setq i (1+ i)))
		     t))))
	     ;;
	     (t;; Normal character
	      (aset normal-feature n chr)
	      ;; `i' was already incremented at the top of the comment removal clause.
	      (setq n (1+ n)))))
	  (br-delete-space
	   (substring normal-feature 0 n)))
      (set-syntax-table original-syntax-table))))

(defun br-feature-display (class-list ftr-pat &optional other-win)
  "Display feature declaration derived from CLASS-LIST, matching FTR-PAT."
  (let  ((classes class-list)
	 (found-ftr)
	 (ftr-sig-regexp)
	 (class)
	 (ftr-tag)
	 (ftr-path))
    (if (or (null class-list) (equal class-list '(nil)))
	nil
      (while (and (not found-ftr) classes)
	(setq class (car classes)
	      ftr-sig-regexp (if (equal br-lang-prefix "objc-")
				 ftr-pat
			       (funcall ftr-pat class))
	      ftr-tag (br-feature-tag-signature-match
		       'identity class ftr-sig-regexp)
	      ftr-path (if ftr-tag (br-feature-tag-path ftr-tag))
	      found-ftr (if ftr-path
			    (br-edit-feature-from-tag
			     ftr-tag ftr-path other-win))
	      classes (if found-ftr nil (cdr classes))))
      (if found-ftr
	  (or class t)
	(br-feature-display
	 (apply 'append (mapcar (function (lambda (cl) (br-get-parents cl)))
				class-list))
	 ftr-pat
	 other-win)))))

(defun br-feature-display-implementors (name)
  "Display the definition of or a list of possible implementors of element NAME.
Return t if one or more are found, nil otherwise."
  (interactive "sImplementors of element named: ")
  (let* ((implementor-tags (br-feature-implementors name))
	 (sig-count (length implementor-tags))
	 tag)
    (cond ((zerop sig-count)
	   (message "(OO-Browser):  No implementor matches for `%s'" name) (beep)
	   nil)
	  ((= sig-count 1)
	   (setq tag (car implementor-tags))
	   (let ((def-file (br-feature-tag-path tag)))
	     (if def-file
		 (if (br-edit-feature-from-tag tag def-file)
		     (progn (message
			     "(OO-Browser):  Found definition of `%s' in class `%s'"
			     name
			     (br-feature-tag-class tag))
			    t))
	       (message
		"(OO-Browser):  No implementor definitions for `%s'" name) (beep)
	       nil)))
	  (t (br-feature-list-implementors implementor-tags name) t))))

(defun br-feature-found-p (buf-file feature-sig-or-tag
			   &optional deferred-class other-win regexp-flag)
  "Search BUF-FILE for FEATURE-SIG-OR-TAG.
BUF-FILE may be a directory in which case the directory is simply displayed.
Return nil if not found, otherwise display it and return the current line number."
  (if buf-file
      (let ((found-def)
	    (opoint (point))
	    (prev-buf)
	    (prev-point)
	    (config (current-window-configuration)))
	(setq prev-buf (get-file-buffer buf-file))
	(funcall br-edit-file-function buf-file other-win)
	(if (file-directory-p buf-file)
	    (setq found-def (file-readable-p buf-file))
	  (setq prev-point (point))
	  (widen)
	  (goto-char (point-min))
	  (setq found-def 
		(cond ((or (null feature-sig-or-tag)
			   (and (br-feature-tag-p feature-sig-or-tag)
				(null (br-feature-tag-signature feature-sig-or-tag))))
		       ;; Tag simply points to the file displayed above.
		       t)
		      (deferred-class
			(br-feature-locate-p feature-sig-or-tag deferred-class))
		      (regexp-flag
		       (br-feature-locate-p feature-sig-or-tag regexp-flag))
		      (t (br-feature-locate-p feature-sig-or-tag)))))
	(if found-def
	    (progn (setq found-def (br-line-number))
		   ;; Set appropriate mode for file.
		   (br-major-mode))
	  (setq buf-file (get-file-buffer buf-file))
	  (if prev-buf
	      (goto-char prev-point)
	    (if buf-file
		(kill-buffer buf-file)
	      (goto-char prev-point)))
	  (br-set-window-configuration config)
	  (goto-char opoint))
	found-def)))

(defun br-feature-list-attributes (class)
  "Return sorted list of attribute tags lexically defined in CLASS."
  (delq nil 
	(mapcar
	 (function (lambda (tag)
		     (if (string-match (concat "\\`" br-attribute-type-regexp)
				       (br-feature-tag-name tag nil t))
			 tag)))
	 (hash-get class br-features-htable))))

(defun br-feature-list-routines (class)
  "Return sorted list of routine tags lexically defined in CLASS."
  (delq nil
	(mapcar
	 (function (lambda (tag)
		     (if (string-match (concat "\\`" br-routine-type-regexp)
				       (br-feature-tag-name tag nil t))
			 tag)))
	 (hash-get class br-features-htable))))

(defun br-feature-map-class-tags (function class)
  "Apply FUNCTION to each feature tag from CLASS and return the non-nil results."
  (delq nil (mapcar function (hash-get class br-features-htable))))

(defun br-feature-map-all-tags (function)
  "Apply FUNCTION to all current feature tags and return a list of the non-nil results."
  (delq nil
	(apply 'nconc
	       (hash-map
		(function
		 (lambda (tag-list-and-class)
		   (mapcar (function (lambda (tag)
				       (funcall function tag)))
			   (car tag-list-and-class))))
		br-features-htable))))

(defun br-feature-map-tags (function regexp)
  "Apply FUNCTION to all current feature tags whose feature name listing entries match REGEXP.
Return a list of the non-nil results."
  (delq nil
	(apply 'nconc
	       (hash-map
		(function
		 (lambda (tag-list-and-class)
		   (mapcar (function
			    (lambda (tag)
			      (if (string-match regexp (br-feature-tag-name tag nil t))
				  (funcall function tag))))
			   (car tag-list-and-class))))
		br-features-htable))))

(defun br-feature-match-implementors (class feature-name)
  "Return a list of exact matching feature tags for CLASS and FEATURE-NAME."
  (let ((match-regexp (concat "\\`" br-feature-type-regexp " " feature-name "\\'"))
	(case-fold-search))
    (if (equal br-lang-prefix "c++-")
	;; Eliminate friend member matches.
	(setq match-regexp (hypb:replace-match-string "%" match-regexp "" t)))
    (br-feature-map-class-tags  
     (function (lambda (tag)
		 (if (string-match match-regexp
				   (br-feature-tag-name tag nil t))
		     tag)))
     class)))

(defun br-feature-name (ftr-entry)
  "Return name part of FTR-ENTRY."
  (cond ((string-equal br-lang-prefix "python-")
	 (if (equal (string-match python-feature-entry-regexp ftr-entry) 0)
	     (substring ftr-entry (match-beginning 2))
	   ""))
	((equal (string-match br-feature-entry ftr-entry) 0)
	 (substring ftr-entry (match-beginning 1)))
	(t "")))

(defun br-feature-set-tags-buffer ()
  "Make the `br-feature-tags-buffer' the current buffer during the current command."
  (if (buffer-live-p br-feature-tags-buffer)
      (set-buffer br-feature-tags-buffer)
    (setq br-feature-tags-buffer
	  (set-buffer (funcall br-find-file-noselect-function
			       br-feature-tags-file)))))

(defun br-feature-signature (&optional arg)
  "Show the full feature signature in the viewer window.
With optional prefix ARG, display signatures of all features from the current
listing buffer."
  (interactive "P")
  (let* ((buf (buffer-name))
	 (owind (selected-window))
	 (tag-list (delq nil (if arg
				     (br-feature-get-tags)
				   (list (br-feature-get-tag))))))
    (if (null tag-list)
	(progn (beep) (message "No elements."))
      (br-to-view-window)
      (switch-to-buffer (get-buffer-create (concat buf "-Elements")))
      (setq buffer-read-only nil)
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (mapcar (function (lambda (tag)
			  (prin1 tag (current-buffer))
			  (terpri (current-buffer))))
	      tag-list)
      (br-major-mode)
      (goto-char 1)
      (select-window owind)
      (message ""))))

(defun br-feature-tag-and-file (class-and-feature-name)
  "Return (feature-tag . feature-def-file-name) of CLASS-AND-FEATURE-NAME.
CLASS-AND-FEATURE-NAME should be given as class::feature-name."
  (let ((case-fold-search)
	class name-regexp)
    ;; Find only exact matches
    (if (string-match "::" class-and-feature-name)
	(setq class (substring class-and-feature-name 0 (match-beginning 0))
	      name-regexp (format "\\`%s %s\\'"
				  br-feature-type-regexp
				  (regexp-quote
				   (substring class-and-feature-name
					      (match-end 0)))))
      ;; Safety fallback, generally should not be used.
      (setq name-regexp (format "\\`%s %s\\'"
				br-feature-type-regexp
				(regexp-quote class-and-feature-name))))
    (catch 'found
      (mapcar
       (function (lambda (tag)
		   (if (string-match name-regexp
				     (br-feature-tag-name tag nil t))
		       (throw 'found
			      (cons tag (br-feature-tag-path tag))))))
       (hash-get class br-features-htable)))))

(defun br-feature-tag-class (tag)
  "Return from TAG the class in which a feature is defined."
  (aref tag 0))

(defun br-feature-tag-sort-list (feature-tags)
  "Sort and return a list of FEATURE-TAGS."
  (let ((standard-output (get-buffer-create " *Feature Tags*")))
    (save-excursion
      (set-buffer standard-output) (setq buffer-read-only nil) (erase-buffer)
      (mapcar (function (lambda (tag) (prin1 tag) (terpri))) feature-tags)
      (call-process-region (point-min) (point-max) "sort" t t nil)
      (goto-char (point-max))
      (princ "\n\)\n")
      (goto-char (point-min))
      (princ "\(\n")
      (goto-char (point-min))
      (prog1 (read (current-buffer))
	(set-buffer-modified-p nil)
	(kill-buffer standard-output)))))

(defun br-feature-tag-name (tag &optional with-class for-display)
  "Return from TAG the name of its feature.
The feature's class name is dropped from the name unless optional WITH-CLASS
is non-nil.  If optional FOR-DISPLAY is non-nil, the feature's type character
is prepended to the name for display in a browser listing."
  (let ((name (aref tag 1)))
    (or for-display (setq name (substring name 2)))
    (if with-class
	(if (equal br-lang-prefix "objc-")
	    (setq name (concat (br-feature-tag-class tag)
			       objc-type-tag-separator name))
	  (setq name (concat (br-feature-tag-class tag) "::" name))))
    name))

(defun br-feature-tag-p (object)
  "Return t if OBJECT is a feature tag, nil otherwise.
The predicate used is relatively loose."
  (and (vectorp object) (= (length object) 4)))

(defun br-feature-tag-path (tag)
  "Return from TAG the pathname of the file in which its feature is defined."
  (hash-get (aref tag 3) br-feature-paths-htable))

(defun br-feature-tag-signature (tag)
  "Return from TAG the source code signature of its feature."
  (or (aref tag 2)
      (if (string-equal br-lang-prefix "python-")
	  ;; If this is a Python tag, since it contains no signature, it must be
	  ;; a module or a package tag; return nil in such a case.
	  nil
	;; Some languages don't store signatures since the feature name is
	;; unique per class.  In such cases, return the feature-name with its
	;; category prefix.
      (br-feature-tag-name tag nil t))))

(defun br-feature-tag-signature-match (function class regexp)
  "Apply FUNCTION to the first feature tag from CLASS whose signature matches REGEXP and return the result.
Return nil if no matching feature tag is found."
  (catch 'found
    (mapcar (function
	     (lambda (tag)
	       (if (and tag (string-match regexp (br-feature-tag-signature tag)))
		   (throw 'found (funcall function tag)))))
	    (hash-get class br-features-htable))
    nil))

(defun br-feature-tags-delete (class)
  "Delete all feature tags lexically defined in CLASS."
  (hash-delete class br-features-htable)
  nil)

(defun br-list-features (class &optional indent)
  "Return sorted list of feature tags lexically defined in CLASS.
Optional INDENT is used in C++ Environments only.  INDENT > 2 indicates that
this is a listing of inherited features, in which case, friend features,
which are never inherited, are omitted from the returned list."
  (if (or (not (equal br-lang-prefix "c++-"))
	  (null indent) (<= indent 2))
      (hash-get class br-features-htable)
    (let ((match-regexp (concat "\\`" br-feature-type-regexp))
	  (case-fold-search))
      ;; Omit C++ friend features which are not inherited since indent > 2.
      (setq match-regexp (hypb:replace-match-string "%" match-regexp "" t))
      (delq nil
	    (mapcar
	     (function (lambda (tag)
			 (if (string-match match-regexp
					   (br-feature-tag-name tag nil t))
			     tag)))
	     (hash-get class br-features-htable))))))

;;;
;;; OO-Browser V3 Legacy Functions Still Used
;;;
(defun br-feature-v3-def-file (feature-tag-regexp)
  "Return FEATURE-DEF-FILENAME for the first OO-Browser V3 tag match of FEATURE-TAG-REGEXP, or nil.
Feature tags come from the file named by `br-feature-tags-file'.

Called exclusively by (smart-element)."
  (save-excursion
    (br-feature-set-tags-buffer)
    (br-feature-v3-def-file-internal feature-tag-regexp)))

(defun br-feature-v3-def-file-internal (feature-regexp)
  "Return file name for the OO-Browser V3 feature matching FEATURE-REGEXP, if any.
Assume feature tags file is current buffer and leave point at the start of
matching feature tag, if any."
  (goto-char 1)
  (and (re-search-forward feature-regexp nil t)
       ;; This ensures that point is left on the same line as the feature tag
       ;; which is found.
       (goto-char (match-beginning 0))
       (br-feature-v3-file-of-tag)))

(defun br-feature-v3-file-of-tag ()
  "Return the file name for the OO-Browser V3 tag that point is within.
Assumes the tag table is the current buffer.

Called exclusively by (smart-element)."
  (save-excursion
    (search-backward "\f" nil t)
    (forward-line 1)
    (let ((start (point)))
      (end-of-line)
      (br-buffer-substring start (point)))))

;;; ************************************************************************
;;; Listing buffer entry tag property handling.
;;; ************************************************************************

(if (string-lessp "19" emacs-version)
    (progn
      ;;
      ;; Emacs 19 or higher buffer entry tags functions
      ;;
(defun br-feature-add-tag (ftr-tag &optional buffer)
  "Add FTR-TAG as a property of the existing line."
  (end-of-line)
  (br-feature-put-property (- (point) 2) (point) 'tag ftr-tag buffer))

(defun br-feature-clear-tags (&optional buf-nm)
  "Erase any feature signatures saved with current buffer or optional BUF-NM."
  (save-excursion
    (if buf-nm (set-buffer (get-buffer buf-nm)))
    (save-restriction
      (widen)
      (remove-text-properties (point-min) (point-max) '(tag)))))

(defun br-feature-get-tag (&optional line-num-minus-one)
  (save-excursion
    (if (numberp line-num-minus-one)
	(goto-line (1+ line-num-minus-one)))
    (end-of-line)
    (car (cdr (memq 'tag (text-properties-at (1- (point))))))))

(defun br-feature-get-tags ()
  (save-excursion
    (goto-char (point-max))
    (let ((found t)
	  (tags)
	  tag)
      (while found
	(setq tag (get-text-property (1- (point)) 'tag))
	(if tag (setq tags (cons tag tags)))
	(setq found (= (forward-line -1) 0))
	(end-of-line))
      tags)))

(if (fboundp 'put-nonduplicable-text-property)
    ;; InfoDock and XEmacs
(defalias 'br-feature-put-property 'put-nonduplicable-text-property)
  ;; GNU Emacs
  (defalias 'br-feature-put-property 'put-text-property))

;; Tag property is placed at end of line in case leading indent is
;; removed by an OO-Browser operation.  In that case, we don't want to
;; lose the tag property.
(defun br-feature-put-tags (ftr-tags)
  (while ftr-tags
    (end-of-line)
    (br-feature-put-property (- (point) 2) (point) 'tag (car ftr-tags))
    (setq ftr-tags (cdr ftr-tags))
    (if (and ftr-tags (/= (forward-line 1) 0))
	(error "(br-feature-put-tags): Too few lines in this buffer"))))

(defun br-feature-to-tag (&optional start end)
  "Move point to the first feature tag property between optional START and END.
Defaults are the start and end of the buffer."
  (goto-char (or (text-property-not-all (or start (point-min))
					(or end (point-max))
					'tag nil)
		 (point))))
      )

  ;;
  ;; Emacs 18 buffer entry tags functions
  ;;

  (defun br-feature-clear-tags (&optional buf-nm)
    "Erase any feature signatures saved with current buffer or optional BUF-NM."
    (put (intern (or buf-nm (buffer-name))) 'features nil))

  (defun br-feature-get-tag (&optional line-num)
    (or (numberp line-num)
	(save-excursion
	  (beginning-of-line)
	  (setq line-num (count-lines 1 (point)))))
    (cdr (assq line-num (get (intern-soft (buffer-name)) 'features))))

  (defun br-feature-get-tags ()
    (get (intern-soft (buffer-name)) 'features))

  (defun br-feature-put-tags (ftr-tags)
    (beginning-of-line)
    (let* ((line (count-lines 1 (point)))
	   (meth-alist (mapcar (function
				(lambda (meth)
				  (prog1 (cons line meth)
				    (setq line (1+ line)))))
			       ftr-tags))
	   (buf-sym (intern (buffer-name))))
      (put buf-sym 'features
	   (nconc (get buf-sym 'features) meth-alist))))
)

;;; ************************************************************************
;;; END - Listing buffer entry tag property handling.
;;; ************************************************************************

(defun br-feature-tags-init (env-file)
  "Set up `br-feature-tags-file' for writing."
  (or env-file (setq env-file br-env-file))
  (setq br-feature-tags-completions nil
	br-element-tags-completions nil
	br-feature-tags-file (br-feature-tags-file-name env-file)
	br-tags-tmp-file (concat env-file "-ETAGS"))
  (br-feature-set-tags-buffer)
  (buffer-disable-undo (current-buffer))
  (make-local-variable 'make-backup-files)
  (make-local-variable 'backup-inhibited)
  (setq make-backup-files nil
	backup-inhibited t
	buffer-read-only nil))

(defun br-feature-tags-file-name (env-file)
  (concat env-file "-FTR"))

(defun br-feature-build-htables ()
  "Filter out extraneous lines from feature tag entries and save `br-feature-tags-file'."
  (save-excursion
    (br-feature-set-tags-buffer)
    (save-buffer) ;; do a temporary save in case there is a failure below
    (c-build-element-tags)
    (goto-char (point-min))
    (delete-matching-lines "^[ \t]*$")
    (goto-char (point-min))
    (replace-regexp "^[ \t]+\\|[ \t]+$" "")
    (save-buffer)
    (br-feature-make-htables)
    (kill-buffer (current-buffer)))
  ;; The feature tags files has been replaced by feature alists stored in
  ;; main Env file, so delete it after extracting its data.
  (if (and (file-exists-p br-feature-tags-file)
	   (file-writable-p br-feature-tags-file))
      (delete-file br-feature-tags-file)))

(defun br-insert-features (feature-tag-list &optional indent)
  "Insert feature names from FEATURE-TAG-LIST in current buffer indented INDENT columns."
  (let ((start (point)))
    (mapcar (function
	     (lambda (feature-tag)
	       (if feature-tag
		   (progn (if indent (indent-to indent))
			  (insert (br-feature-tag-name feature-tag nil t)
				  "\n")))))
	    feature-tag-list)
    (save-excursion
      (goto-char start)
      (br-feature-put-tags feature-tag-list))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun br-feature-current ()
  "Extract current feature from tags file and leave point at the end of line."
  (beginning-of-line)
  (br-buffer-substring (point) (progn (end-of-line) (point))))

(defun br-feature-insert-ancestor-implementors (class-list feature-name
					        &optional depth offset count)
  "Insert into the current buffer ancestor implementor listings matching CLASS-LIST and FEATURE-NAME.
Ancestor trees are inverted, i.e. parents appear below children, not above.
Indent each class in CLASS-LIST by optional DEPTH spaces (default is 0 in
order to ensure proper initialization).  Offset each child level by optional
OFFSET spaces from its parent (which must be greater than zero, default 2).
COUNT is the number of implementers found.  This number is returned to the
caller."
  (or offset (setq offset 2))
  (or depth (setq depth 0))
  (or count (setq count 0))
  (if (= depth 0) (setq br-tmp-class-set nil))
  (let ((prev-expansion-str " ...")
	parents expand-subtree tags)
    (mapcar
      (function
	(lambda (class)
	  (setq expand-subtree (br-set-cons br-tmp-class-set class)
		parents (if expand-subtree (br-get-parents class)))
	  (indent-to depth)
	  (insert class)
	  (and (not expand-subtree) (br-has-children-p class)
	       (insert prev-expansion-str))
	  (insert "\n")
	  (if (not expand-subtree) ;; repeated class
	      nil
	    ;; Compute implementors list
	    (setq tags (br-feature-match-implementors class feature-name))
	    (setq count (+ count (br-feature-insert-signatures
				  tags
				  ;; Indent implementors twice as much
				  ;; as class names for readability.
				  (+ depth offset offset)))))
	  (if parents
	      (setq count (+ count
			     (br-feature-insert-ancestor-implementors
			      parents feature-name (+ depth offset)
			      offset 0))))))
      class-list))
  (if (= depth 0) (setq br-tmp-class-set nil))
  count)

(defun br-feature-insert-descendant-implementors (class-list feature-name
					          &optional depth offset count)
  "Insert into the current buffer descendant implementor listings matching CLASS-LIST and FEATURE-NAME.
Indent each class in CLASS-LIST by optional DEPTH spaces (default is 0 in
order to ensure proper initialization).  Offset each child level by optional
OFFSET spaces from its parent (which must be greater than zero, default 2).
COUNT is the number of implementers found.  This number is returned to the
caller."
  (or offset (setq offset 2))
  (or depth (setq depth 0))
  (or count (setq count 0))
  (if (= depth 0) (setq br-tmp-class-set nil))
  (let ((prev-expansion-str " ...")
	children expand-subtree tags)
    (mapcar
      (function
	(lambda (class)
	  (setq expand-subtree (br-set-cons br-tmp-class-set class)
		children (if expand-subtree (br-get-children class)))
	  (indent-to depth)
	  (insert class)
	  (and (not expand-subtree) (br-has-children-p class)
	       (insert prev-expansion-str))
	  (insert "\n")
	  (if (not expand-subtree) ;; repeated class
	      nil
	    ;; Compute implementors list
	    (setq tags (br-feature-match-implementors class feature-name))
	    (setq count (+ count (br-feature-insert-signatures
				  tags
				  ;; Indent implementors twice as much
				  ;; as class names for readability.
				  (+ depth offset offset)))))
	  (if children
	      (setq count (+ count
			     (br-feature-insert-descendant-implementors
			      children feature-name (+ depth offset)
			      offset 0))))))
      class-list))
  (if (= depth 0) (setq br-tmp-class-set nil))
  count)

(defun br-feature-insert-signatures (tag-list indent)
  "Insert feature signatures from feature TAG-LIST into current buffer indented INDENT columns.
Return the number of feature signatures inserted."
  (let ((start (point)))
    (mapcar (function (lambda (tag)
			(indent-to indent)
			(insert (br-feature-tag-signature tag) "\n")))
	    tag-list)
    (save-excursion
      (goto-char start)
      (br-feature-put-tags tag-list)))
  (length tag-list))

(defun br-feature-list-implementors (implementors name)
  "Display a buffer with a list of known IMPLEMENTORS of an element NAME." 
  (interactive (list nil (read-string "List implementors of element named: ")))
  (let ((temp-buffer-show-function temp-buffer-show-function)
	(prev-class) class sig)
    (if (br-in-browser)
	(progn (br-to-view-window)
	       (setq temp-buffer-show-function 'switch-to-buffer)))
    (with-output-to-temp-buffer "*Implementors*"
      ;; Next line needed because of call to `br-feature-add-tag' below.
      (set-buffer standard-output)
      (princ "Press the Action Key on any line below to display its definition:")
      (terpri) (terpri)
      (mapcar (function (lambda (tag)
			  (setq class (br-feature-tag-class tag))
			  (if (not (equal class prev-class))
			      (progn (princ class) (terpri)
				     (setq prev-class class)))
			  (setq sig (br-feature-tag-signature tag))
			  (princ "  ") (princ sig)
			  (br-feature-add-tag tag standard-output)
			  (terpri)))
	      (or implementors (br-feature-implementors name))))
    (select-window (or (get-buffer-window "*Implementors*") (selected-window)))
    (forward-line 2)))

(defun br-feature-make-htables ()
  "Convert the current buffer of OO-Browser feature tags to hash table entries."
  (message "Building class features index...")
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((path-counter 1)
	    (paths-alist)
	    (features-alist)
	    (end-of-file-entries)
	    (python (string-equal br-lang-prefix "python-"))
	    (standard-output (get-buffer-create "*br-feature-alists*"))
	    class entry path signature)
	(save-excursion
	  (set-buffer standard-output) (setq buffer-read-only nil)
	  (erase-buffer))
	(condition-case ()
	    (while t
	      (forward-line 1) ;; past ^L separator
	      (setq path (read (current-buffer)))
	      (if (not (stringp path)) (setq path (symbol-name path)))
	      (setq paths-alist (cons (cons (int-to-string path-counter)
					    path)
				      paths-alist))
	      (save-excursion
		(setq end-of-file-entries
		      (1- (or (search-forward "\^L" nil t)
			      (point-max)))))
	      (forward-line 1) ;; past pathname
	      (while (< (point) end-of-file-entries)
		(if (looking-at br-tag-fields-regexp)
		    (progn
		      (setq class (buffer-substring
				   (match-beginning 1) (match-end 1))
			    entry (buffer-substring
				   ;; Grouping 2 match may not exist.
				   (or (match-beginning 2)
				       (match-beginning 3))
				   (match-end 3)))
		      (end-of-line)
		      (if (= (match-end 0) (point))
			  ;; No signature
			  (setq signature nil)
			(setq signature (buffer-substring
					 (match-end 0)
					 (progn (end-of-line) (point))))
			(if python
			    ;; Add module name to listing entry.
			    (setq entry
				  (concat
				   (substring entry 0 2)
				   (python-module-name path) "."
				   (substring entry 2)))))
		      (princ (format "(%S . [%S %S %S \"%d\"])\n"
				     class class entry signature path-counter)))
		  (error "(OO-Browser):  Invalid feature entry, `%s'"
			 (buffer-substring
			  (point) (save-excursion (end-of-line) (point)))))
		(forward-line 1))
	      (setq path-counter (1+ path-counter)))
	  (end-of-file nil))

	(setq paths-alist
	      ;; This entry appears as the reverse of all others so that
	      ;; we can use the literal "path-counter" as a key to look up
	      ;; the current count.
	      (cons (cons "path-counter" (int-to-string path-counter))
		    (nreverse paths-alist)))

	(set-buffer standard-output)
	(if (stringp br-sort-options)
	    ;; Sort in dictionary order using only alpha characters so that
	    ;; feature type entry characters do not influence the ordering.
	    (call-process-region (point-min) (point-max) "sort" t t nil "-rd")
	  ;; MS OSes
	  (call-process-region (point-min) (point-max) "sort" t t)
	  (reverse-region (point-min) (point-max)))
	(goto-char (point-min))
	(princ "\(setq features-alist\n'\(\n")
	(goto-char (point-max))
	(princ "\n\)\)\n")
	(goto-char (point-min))
	;; set feature alist variables
	(eval (read (current-buffer)))
	(set-buffer-modified-p nil)
	(kill-buffer standard-output)
	(setq br-features-htable (hash-make-prepend features-alist t)
	      br-feature-paths-htable (hash-make paths-alist t)))))
  (message "Building class features index...Done"))

(defun br-feature-relation-implementors (class-name feature-name
					 implementors-function method-flag)
  "Display an *Implementors* buffer with a subset of implementor listings related to CLASS-NAME and FEATURE-NAME, computed from IMPLEMENTORS-FUNCTION.
The feature is a method if METHOD-FLAG is non-nil, otherwise, it is an attribute.
Return the number of implementors found."
  (message "Locating definition matches for %s::%s..."
	   class-name feature-name)
  (let ((temp-buffer-show-function temp-buffer-show-function)
	count
	obuf)
    (if (br-in-browser)
	(progn (br-to-view-window)
	       (setq temp-buffer-show-function 'switch-to-buffer)))
    (setq obuf (current-buffer))
    (set-buffer (get-buffer-create "*Implementors*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert
     "Press the Action Key on any line below to display its definition:\n\n")
    (setq count (funcall implementors-function (list class-name) feature-name))
    (cond ((zerop count)
	   ;; No implementors found
	   (message "")
	   (set-buffer-modified-p nil (get-buffer "*Implementors*"))
	   (kill-buffer "*Implementors*")
	   (set-buffer obuf))
	  ((= count 1)
	   ;; Jump to definition and delete *Implementors* buffer.
	   (br-feature-to-tag)
	   (let* ((ftr-tag (br-feature-get-tag))
		  (ftr-class (br-feature-tag-class ftr-tag))
		  (ftr-path (br-feature-tag-path ftr-tag)))
	     (if (and ftr-path (br-edit-feature-from-tag ftr-tag ftr-path))
		 (progn (message "(OO-Browser):  Found the %sdefinition of %s::%s"
				 (if (equal class-name ftr-class)
				     "" "inherited ")
				 class-name feature-name)
			nil)
	       (set-buffer-modified-p nil (get-buffer "*Implementors*"))
	       (kill-buffer "*Implementors*")
	       (set-buffer obuf)
	       (if ftr-path
		   ;; ftr-tag not found within ftr-path; this means some
		   ;; directory or file name within the Environment data
		   ;; files is out of sync with the actual directory or file
		   ;; name in use locally, e.g. when an Environment is copied
		   ;; from one system to another and the Environment
		   ;; directories are not updated.
		   (with-output-to-temp-buffer "*OO-Browser Error*"
		     (princ "The OO-Browser found an entry for `")
		     (princ feature-name)
		     (princ "'\nbut could not find the actual definition within\n")
		     (princ "the source file which is supposed to define the feature.\n\n")
		     (princ "The current OO-Browser Environment is defined by the file:\n")
		     (princ "  ")
		     (prin1 br-env-file)
		     (terpri) (terpri)
		     (princ "The Environment file mistakenly says that\n`")
		     (princ feature-name)
		     (princ "' is defined within the file:\n")
		     (princ "  ")
		     (prin1 ftr-path)
		     (terpri) (terpri)
		     (princ "If this source file does not exist, the cause is often\n")
		     (princ "that an OO-Browser Environment has been copied from\n")
		     (princ "one directory or one machine to another.  In that case,\n")
		     (princ "you should delete the Environment file and then\n")
		     (princ "re-create it.\n"))

		 ;; This next case should never really happen.  It means the
		 ;; ftr-tag did not match to a file name within the
		 ;; OOBR-FTR file.
		 (message
		  "(OO-Browser):  No implementor definitions for `%s'" feature-name)
		 (beep)))))
	  (t
	   ;; Display *Implementors* buffer for user selection.
	   (br-pop-to-buffer "*Implementors*")
	   (goto-char (point-min))
	   (forward-line 2)
	   (message "(OO-Browser):  %d definitions of %s::%s found"
		    count class-name feature-name)))
    count))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defconst br-feature-entry-regexp
  (concat br-feature-type-regexp " \\([^\t\n\r]*[^ \t\n\r]\\)")
  "Regexp matching a feature entry string from a browser listing buffer.")

(defvar br-default-class-tags-completions nil
  "List of (envir-name tags-file-last-mod-time default-class-tags-completion-alist).")

(defvar br-element-tags-completions nil
  "List of (envir-name tags-file-last-mod-time elt-tags-completion-alist).")

(defvar br-feature-tags-completions nil
  "List of (envir-name tags-file-last-mod-time ftr-tags-completion-alist).")

(defvar br-feature-tags-file nil
  "Pathname where object-oriented feature tags are temporarily stored during Environment builds.")

(defvar br-feature-tags-buffer nil
  "Cached buffer attached to `br-feature-tags-file'.")

(defvar br-tags-tmp-file nil
  "Temporary pathname used to compute non-object-oriented feature tags.")

(provide 'br-ftr)
