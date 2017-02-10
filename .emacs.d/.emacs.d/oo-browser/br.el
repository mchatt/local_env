;;!emacs
;;
;; FILE:         br.el
;; SUMMARY:      Browse object-oriented code.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     matching, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:    12-Dec-89
;; LAST-MOD:     10-May-01 at 20:49:01 by Bob Weiner
;;
;; Copyright (C) 1989-1998  BeOpen.com
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

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defconst br-feature-signature-regexp "[:|,]"
  "Regular expression that matches a feature signature but not a class name.")

(defvar br-inherited-features-flag t
  "*If non-nil (the default), feature/element listings include all inherited features.
If nil, only those features lexically included within a class are shown.")

(defvar br-protocols-with-classes-flag t
  "*If non-nil (the default), protocols (interfaces) are included in listings of all classes or top-level classes.")

(defvar br-inhibit-version nil
  "*Personal setting which if non-nil, skips version/credit information upon startup.
The default should be left as nil, since new users may find this helpful.")

(defvar br-invert-ancestors nil
  "*Personal setting which if non-nil makes ancestors appear as do other inheritance listings.
That is, parents appear above children, rather than the default, which is the
reverse.")

(defvar br-keep-viewed-classes nil
  "*Personal setting which if non-nil means leave all viewed classes around for later selection.  
A nil value causes deletion of the last viewed class buffer whenever a new
one is displayed.   Note this does not affect classes displayed for editing,
all such classes are left around.")

(defconst br-min-width-window 25
  "*Minimum width of a browser class list window.
This together with the frame width determines the number of such windows.")

;;; ************************************************************************
;;; Public macros
;;; ************************************************************************

(if (fboundp 'window-highest-p)
    (defun br-listing-window-p ()
      "Is the selected window an OO-Browser listing window?"
      (if (br-in-browser) (window-highest-p (selected-window))))
  (defun br-listing-window-p ()
    "Is the selected window an OO-Browser listing window?"
    ;; Top of window is at top of frame.
    (if (br-in-browser)
	(= (nth 1 (window-edges)) br-top-of-frame))))

(defun br-non-listing-window-p ()
  "Is the selected window a non-OO-Browser listing window?"
  (not (br-listing-window-p)))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun br-browse ()
  "Internally invoke the OO-Browser, for browsing class hierarchies.
Use \\[br-help] and \\[br-help-ms] for help on browser usage."
  (interactive)
  (if (fboundp 'get-frame-for-mode)
      (let ((frame (get-frame-for-mode 'br-mode)))
	(if (frame-live-p frame)
	    (if (frame-visible-p frame)
		(select-frame frame)
	      (make-frame-visible frame)
	      (select-frame frame)))))
  ;; If not already in the browser, save window config.
  (if (br-in-browser)
      nil
    (let* ((env-build-process
	    (or (and (boundp 'compilation-in-progress)
		     (car compilation-in-progress))
		(get-buffer-process "*compilation*")))
	   (env-being-built
	    (and env-build-process
		 (boundp 'compile-command)
		 (stringp compile-command)
		 (string-match "oo-browser-env" compile-command)
		 (eq (process-status env-build-process) 'run)
		 (equal (getenv "OO_BROWSER_ENV") br-env-file))))
      (setq *br-prev-wconfig* (current-window-configuration)
	    br-in-browser (selected-frame))
      ;; If were previously in the browser, restore its saved window config,
      ;; otherwise, set up from scratch.
      (if (and *br-save-wconfig*
	       (br-set-window-configuration *br-save-wconfig*)
	       (br-in-browser))
	  nil
	(br-window-setup)
	(cond ((not env-being-built)
	       (br-version)
	       (message "Press {h} for help; use {C-c #} to see version and credits again.")
	       (br-show-all-classes)
	       (message "Press {h} for help; use {C-c #} to see version and credits again.")
	       ;; Wait for 60 seconds or until a single key sequence is given.
	       (sit-for 60)
	       (message ""))
	      (br-inhibit-version
	       (br-show-all-classes)))
	(if env-being-built nil	(br-help)))
      (if env-being-built
	  (let ((owind (selected-window)))
	    (br-to-view-window)
	    (switch-to-buffer "*compilation*")
	    (goto-char (point-max))
	    (select-window owind)
	    (message "Waiting for build to finish before loading Environment ...")))
      (run-hooks 'br-mode-hook
		 (intern (concat "br-" br-lang-prefix "mode-hook"))))))

;;;  ###autoload
;(defun br-add-class-file (&optional class-path lib-table-p save-file)
;  "Add a file of classes to the current Environment.
;Interactively or when optional CLASS-PATH is nil, CLASS-PATH defaults to the
;current buffer file pathname.  If optional LIB-TABLE-P is non-nil, add to
;Library Environment, otherwise add to System Environment.  If optional
;SAVE-FILE is t, the Environment is then stored to the filename given by
;`br-env-file'.  If SAVE-FILE is non-nil and not t, its string value is used
;as the file to which to save the Environment."
;  (interactive
;   (let ((class-path
;	  (expand-file-name
;	   (read-file-name (concat "Class file name to add"
;				   (if buffer-file-name
;				       (concat " (default \""
;					       (file-name-nondirectory
;						buffer-file-name)
;					       "\")"))
;				   ": ")
;			   nil buffer-file-name t)))
;	 (lib-table-match)
;	 (sys-table-match)
;	 (save-file))
;     (if (equal class-path "")
;	 nil
;       (setq lib-table-match
;	     (delq nil
;		   (mapcar
;		    (function
;		     (lambda (search-dir)
;		       (if (string-match (regexp-quote
;					  (expand-file-name search-dir))
;					 class-path) t)))
;		    br-lib-search-dirs)))
;       (if lib-table-match
;	   nil
;	 (setq sys-table-match
;	       (delq nil
;		     (mapcar
;		      (function
;		       (lambda (search-dir)
;			 (if (string-match (regexp-quote
;					    (expand-file-name search-dir))
;					   class-path) t)))
;		      br-sys-search-dirs)))))
;     (if (or lib-table-match sys-table-match)
;	 nil
;       (setq lib-table-match
;	     (y-or-n-p "Add to Library, rather than System part of Environment? ")))
;     (setq save-file (y-or-n-p
;		      (format "Save %s after this addition? " br-env-file)))
;     (list class-path lib-table-match save-file)))

;  (or class-path (setq class-path buffer-file-name))
;  (if (not (if class-path (file-readable-p class-path)))
;      (error "(br-add-class-file): %s is not readable" class-path))
;  (let* ((paths-parents-cons
;	   (let ((br-view-file-function 'br-insert-file-contents))
;	     (br-get-classes-from-source class-path)))
;	 (classes (car paths-parents-cons))
;	 (parents (cdr paths-parents-cons))
;	 (paths-key class-path)
;	 (path-htable (br-get-htable (if lib-table-p "lib-paths" "sys-paths")))
;	 (par-htable (br-get-htable
;		       (if lib-table-p "lib-parents" "sys-parents")))
;	 (child-htable (br-get-children-htable)))
;    (mapcar
;      (function
;	(lambda (class)
;	  (br-add-to-paths-htable class paths-key path-htable)))
;      classes)
;    (mapcar
;      (function
;	(lambda (parent-cons)
;	  (hash-add (car parent-cons) (cdr parent-cons) par-htable)))
;      parents)
;    (br-env-set-htables t)
;    (let ((child) (par-list) children)
;      (mapcar
;	(function
;	  (lambda (parent-cons)
;	    (setq child (cdr parent-cons)
;		  par-list (car parent-cons))
;	    (mapcar
;	      (function
;		(lambda (parent)
;		  (setq children (hash-get parent child-htable))
;		  (or (br-member child children)
;		      (hash-add (cons child children) parent child-htable))))
;	      par-list)))
;	parents)))
;  (cond ((eq save-file nil))
;	((eq save-file t) (br-env-save))
;	((br-env-save save-file))))

(defun br-ancestors (&optional arg features-string concrete-classes-flag)
  "Display ancestor tree whose root is the current class.
With optional prefix ARG, display all ancestor trees whose roots are in the
current listing.  With no ARG or if ARG = -1 or `br-invert-ancestors' is t,
the current class ancestry tree is inverted.  That is, it shows branches
going down towards the root class, so that parents appear above children.  If
ARG < -1 or `br-invert-ancestors' is t and ARG > 1, then the ancestry trees
of all classes in the current listing are inverted.

Optional second argument, FEATURES-STRING, is the plural name of the type of
features to display along with each ancestor class.

CONCRETE-CLASSES-FLAG non-nil means omit abstract classes from the tree."
  (interactive "p")
  (or arg (setq arg 1))
  (if br-invert-ancestors (setq arg (- arg)))
  (let* ((class-list
	  (if (and (/= arg 1) (/= arg -1))
	      (br-this-level-classes)
	    (br-find-class-name-as-list)))
	 (parents (delq nil (mapcar (function
				     (lambda (c) (br-get-parents c)))
				    class-list))))
    (cond ((and class-list
		(or parents
		    (and features-string
			 (if (/= 1 (length class-list))
			     t;; Assume some class will have features.
			   ;; This class must have features.
			   (br-list-features (car class-list))))))
	   (if (and (/= arg 1) (/= arg -1))
	       (message "Computing %s..."
			(or features-string "ancestors")))
	   (br-setup-next-window (if features-string
				     (substring features-string 0 1)
				   ;; ancestors
				   "a"))
	   (let (buffer-read-only)
	     (cond ((>= arg 0)
		    (br-ancestor-trees class-list nil nil
				       concrete-classes-flag))
		   (t
		    (br-ancestor-trees-inverted class-list nil nil
						concrete-classes-flag))))
	   (goto-char (point-min))
	   (if (and (/= arg 1) (/= arg -1))
	       (message "Computing %s...Done"
			(or features-string "ancestors")))
	   t)
	  ((null class-list)
	   (message "(OO-Browser):  Apply `br-%s' to a class."
		    (or features-string "ancestors"))
	   (beep))
	  (t
	   (message "No %s." (or features-string "ancestors"))
	   (beep)))))

(defun br-at (&optional arg)
  "Display the current class location in the inheritance graph.
The class is displayed among both its ancestors and descendants.
With optional prefix ARG, display the locations for all classes in the
current listing."
  (interactive "P")
  (let* ((parent)
	 (parent-list
	   (if arg
	       (br-this-level-classes)
	     (list (setq parent (br-find-class-name))))))
    (if arg (message "Computing class locations..."))
    (br-setup-next-window "@")
    (let (buffer-read-only)
      (br-descendant-trees (br-ancestor-roots parent-list))
      (goto-char (point-min))
      (if arg
	  (message "Computing class locations...Done")
	(re-search-forward (concat "\\(^\\|[ \t]+\\)" parent "$"))
	(goto-char (match-end 1))
	(recenter '(4))))))

(defun br-attributes (arg)
  "Display attributes of the current class (prefix ARG = 1) or of the current listing if ARG is other than 0 or 1.

With ARG = 0, the value of the variable, `br-inherited-features-flag', is
toggled and no other action is taken.

If `br-inherited-features-flag' is t, all attributes of each class are shown.
If nil, only lexically included attributes are shown and if the attributes of a
single class are requested and none are defined, the class definition is
displayed so that its attribute declarations may be browsed."
  (interactive "p")
  (cond ((and (integerp arg) (= arg 0))
	 (setq br-inherited-features-flag
	       (not br-inherited-features-flag))
	 (message "Inherited features/elements will %sbe shown."
		  (if br-inherited-features-flag "" "not ")))
	(br-inherited-features-flag
	 (br-inherited-attributes arg))
	(t (br-lexical-attributes arg))))

(defun br-categories (&optional arg)
  "Display categories directly associated with the current class.
This does not include any categories which the class inherits.
With optional prefix ARG, display categories of all classes in the current
listing."
  (interactive "P")
  (let ((has-categories)
	class-list categories class-and-categories)
    (setq class-list (cond (arg
			    (message "Computing class categories...")
			    (br-this-level-classes))
			   (t 
			    (br-find-class-name-as-list)))
	  categories
	  (delq nil (mapcar
		     (function
		      (lambda (class)
			(setq class-and-categories (br-list-categories class)
			      has-categories (or has-categories
						 class-and-categories))
			(cons class class-and-categories)))
		     class-list)))
    (cond ((not class-list)
	   (message "(OO-Browser):  Apply `br-categories' to a class.") (beep))
	  ((not has-categories)
	   (message "No class categories.") (beep))
	  (t
	   (br-setup-next-window "C")
	   (let (buffer-read-only done-set class)
	     (mapcar
	      (function
	       (lambda (class-and-categories)
		 (setq class (car class-and-categories))
		 (if (not (br-set-cons done-set class))
		     (insert class " ...\n")
		   ;; Class successfully added to set, so it has not been
		   ;; listed before.
		   (insert class "\n")
		   (br-insert-features (cdr class-and-categories) 2))))
	      categories))
	   (message "Computing class categories...Done")
	   (goto-char (point-min))
	   t))))

(defun br-children (&optional arg)
  "Display the children of the current class.
With optional prefix ARG, display the children of all the classes in the current
listing."
  (interactive "P")
  (let ((class-list (cond (arg
			   (message "Computing children...")
			   (br-this-level-classes))
			  (t
			   (br-find-class-name-as-list))))
	(has-children)
	children children-list)
    (setq children-list (delq nil (mapcar
				   (function
				    (lambda (parent)
				      (setq children
					    (br-get-children parent)
					    has-children
					    (or has-children children))
				      (cons parent children)))
				   class-list)))
    (cond ((not children-list)
	   (message "(OO-Browser):  Apply `br-children' to a class.")
	   (beep))
	  ((not has-children)
	   (message "No children.") (beep))
	  (t
	   (br-setup-next-window "c")
	   (let (buffer-read-only done-set parent)
	     (mapcar
	      (function
	       (lambda (parent-children-cons)
		 (setq parent (car parent-children-cons))
		 (if (not (br-set-cons done-set parent))
		     (insert parent " ...\n")
		   ;; Class successfully added to set, so it has not been
		   ;; listed before.
		   (insert parent "\n")
		   (br-insert-classes (cdr parent-children-cons) 2))))
	      children-list))
	   (if arg (message "Computing children...Done"))
	   (goto-char (point-min))
	   t))))

(defun br-class-stats (&optional prompt)
  "Display a statistics summary for current class.
Optional prefix arg PROMPT means prompt for a class name."
  (interactive "P")
  (let ((class-name (if prompt (br-complete-class-name) (br-find-class-name))))
    (if class-name
	(message "Class %s:  Parents: %d; Children: %d"
		 class-name (length (br-get-parents class-name))
		 (length (br-get-children class-name)))
      (error "No class name at point."))))

(defun br-cmd-help (key &optional full)
  "Show first line of doc for OO-Browser KEY in minibuffer.
With optional FULL, display full documentation for command."
  (interactive "kOO-Browser key binding: \nP")
  (let* ((cmd (let ((cmd (if (eq major-mode 'br-mode)
			     (lookup-key br-mode-map key)
			   (key-binding key))))
		(if (not (integerp cmd)) cmd)))
	 (doc (and cmd (documentation cmd)))
	 (end-line))
    (if doc
	(or full
	    (setq end-line (string-match "[\n]" doc)
		  doc (substitute-command-keys (substring doc 0 end-line))))
      (setq doc (format "No documentation for {%s} %s" key (or cmd ""))))
    (if (and cmd doc)
	(if full
	    (progn (br-to-view-window)
		   (other-window -1)
		   (describe-function cmd))
	  (message doc)))))

(defun br-count ()
  "Count the number of entries visible in current listing buffer.
Print the text result in the minibuffer when called interactively."
  (interactive)
  (let ((cnt (count-lines (point-min) (point-max))))
    (if (interactive-p)
	(message "%s contains %d entries." (buffer-name) cnt)
      cnt)))

(defun br-copyright ()
  "Display the OO-Browser copyright information in the viewer window."
  (interactive)
  (br-file-to-viewer "BR-COPY"))

(defun br-delete (&optional prompt)
  "Delete a class from the current Environment.
Does not alter descendency relations.
Optional prefix arg PROMPT means prompt for the class name."
  (interactive "P")
  (let ((class (if prompt (br-complete-class-name) (br-find-class-name))))
    (and class
	 (if (interactive-p)
	     (y-or-n-p (concat "Delete class " class " from Environment? "))
	   t)
	 (progn (br-real-delete-class class)
		;; Delete class name at point in listing window
		(or prompt (let (buffer-read-only)
			     (progn (beginning-of-line)
				    (delete-region
				     (point) (progn (forward-line 1)
						    (point))))))
		(message "Class " class " deleted.")))))

(defun br-descendants (&optional arg)
  "Display the descendant tree whose root is the current class.
With optional prefix ARG, display all descendant trees whose roots are
the classes in the current listing."
  (interactive "P")
  (let ((parent-list
	 (if arg
	     (br-this-level-classes)
	   (br-find-class-name-as-list))))
    (cond ((delq nil (mapcar
		      (function (lambda (parent)
				  (br-get-children parent)))
		      parent-list))
	   (if arg (message "Computing descendants..."))
	   (br-setup-next-window "d")
	   (let (buffer-read-only)
	     (br-descendant-trees parent-list))
	   (goto-char (point-min))
	   (if arg (message "Computing descendants...Done"))
	   t)
	  (t
	   (message "No descendants.") (beep)))))

(defun br-edit-entry (&optional prompt)
  "Edit the source code for any browser listing entry, such as a class or a feature.
Optional prefix arg PROMPT means prompt for the entry name; automatically
prompts if called interactively outside of a listing window, e.g. within a
source code buffer when the browser user interface is not displayed."
  (interactive "P")
  (let ((entry) (feature-tag))
    (if (or prompt (and (interactive-p) (not (br-in-browser))))
	(cond ((and (setq entry (br-complete-entry))
		    (string-match br-feature-signature-regexp entry))
	       (if (setq feature-tag (car (br-feature-tag-and-file entry)))
		   (br-feature nil feature-tag)
		 (error "(br-feature-tag-and-file): Could not find match for: `%s'" entry)))
	      (entry  ;; class name
		(br-edit nil entry))
	      (t (error "(br-complete-entry): Exited without selecting a match")))
      (cond ((br-at-feature-p)
	     (br-feature)
	     t)
	    ((and (setq entry (br-find-class-name))
		  (br-class-in-table-p entry))
	     (br-edit nil entry))
	    (t (error "(OO-Browser):  No `%s' entry in the current Environment"
		      entry))))))

(defun br-edit (&optional prompt class)
  "Edit a class in the viewer window.
Select viewer window.  With optional prefix arg PROMPT, prompt for class
name.  Optional CLASS is the one to edit.  Return t if class is displayed or
sent to an external viewer, else nil."
  (interactive "P")
  (or br-editor-cmd
      (not (br-in-browser))
      (br-in-view-window-p)
      (setq *br-prev-listing-window* (selected-window)))
  (br-view prompt t class))

(defun br-edit-ext (editor-cmd file line-num)
  "Invoke a non-standard EDITOR-CMD on FILE at LINE-NUM.
See also `br-editor-cmd'."
  (interactive "fFile to edit: ")
  (or editor-cmd (setq editor-cmd br-editor-cmd))
  (if (not (stringp editor-cmd))
      ;; must be a Lisp function that takes two args, a file and line number
      (funcall editor-cmd file line-num)
    (setq delete-exited-processes t)
    (let ((proc)
	  (name (concat br-ed-name (int-to-string br-ed-num))))
      (setq br-ed-num (1+ br-ed-num)
	    proc (br-edit-ext-start editor-cmd name file line-num))
      (if proc
	  (process-kill-without-query proc)
	(beep)
	(message "(OO-Browser):  Could not start external edit process: %s"
		 editor-cmd)))))

(defun br-editor-kill ()
  "Kill all current external editor sub-processes."
  (interactive)
  (if (br-kill-process-group br-ed-name br-ed-num "external editors")
      (setq br-ed-num 0)))

(defun br-entry-info ()
  "Display in the viewer window documentation for the current listing entry."
  (interactive)
  (if (fboundp 'br-insert-entry-info)

      ;; For languages which use the newer entry-info functions.
      (if (looking-at ".")
	  (progn
	    (message "Building entry info...")
	    (if (br-store-entry-info)
		(progn (message "Building entry info...Done")
		       (br-funcall-in-view-window
			(concat br-buffer-prefix-info "Info*")
			'br-insert-entry-info))
	      (beep)
	      (message "There is no documentation for `%s'."
		       (br-feature-name (br-feature-entry)))))
	(beep)
	(message "Move point to the beginning of an entry name line."))

    ;; For languages which use the older class-info functions.
    (let ((class-name (br-find-class-name)))
      (if class-name
	  (if (fboundp 'br-insert-class-info)
	      (progn
		(message "Building `%s' class info..." class-name)
		(if (br-store-class-info class-name)
		    (progn
		      (message "Building `%s' class info...Done" class-name)
		      (br-funcall-in-view-window
		       (concat br-buffer-prefix-info "Info*")
		       'br-insert-class-info))
		  (beep)
		  (message "There is no documentation for `%s'." class-name)))
	    (beep)
	    (message "No class information function for this language."))
	(beep)
	(message "No entry information function for this language.")))))

(defun br-exit-level (arg)
  "Return to prefix ARGth previous OO-Browser listing level.
The command is ignored with ARG < 1."
  (interactive "p")
  (setq arg (or arg 1))
  (let ((prev-wind-buf-line))
    (if (null *br-level-hist*)
	(and (> arg 0)
	     (message "No previous level to which to exit.")
	     (beep))
      (while (and (> arg 0) *br-level-hist*)
	(br-next-buffer
	 (int-to-string (br-listing-window-num)) br-buffer-prefix-blank)
	(setq prev-wind-buf-line (car *br-level-hist*)
	      *br-level-hist* (cdr *br-level-hist*)
	      arg (1- arg))
	(select-window (car prev-wind-buf-line))
	(switch-to-buffer (car (cdr prev-wind-buf-line))))
      (widen)
      ;; Position window lines exactly as before.
      (recenter (car (cdr (cdr prev-wind-buf-line)))))))

(defun br-feature (&optional view-only feature-tag)
  "Edit a feature in the viewer window.  Select viewer window.
Optional VIEW-ONLY non-nil means view rather than edit feature.
Optional FEATURE-TAG includes the signature of the feature to edit."
  (interactive)
  (or feature-tag
      ;; Get current feature tag
      (setq feature-tag (br-feature-get-tag)))
  (if (null feature-tag)
      (error "(br-feature): No definition for this entry")
    (let ((feature-sig
	   (if (br-feature-tag-p feature-tag)
	       (br-feature-tag-signature feature-tag)
	     feature-tag)))
      (cond ((and (or (and (not view-only) (br-edit-externally-p))
		      (and view-only (br-view-externally-p)))
		  (let* ((func (function
				(lambda (path unused)
				  (set-buffer (find-file-noselect path)))))
			 (br-edit-file-function func)
			 (br-view-file-function func)
			 (file (br-feature-tag-path feature-tag))
			 (line-num (save-excursion
				     (br-feature-found-p file feature-tag))))
		    (and line-num
			 (funcall (if view-only 'br-view-externally
				    'br-edit-externally)
				  file line-num)))))
	    ((progn
	       (br-to-view-window)
	       (br-feature-found-p (br-feature-tag-path feature-tag) feature-tag))
	     (if view-only
		 (progn (setq buffer-read-only t)
			(br-to-from-viewer))
	       (if (and buffer-file-name (file-writable-p buffer-file-name))
		   (setq buffer-read-only nil))))
	    ;;
	    ;; Feature not found.  Return to original window and signal an error.
	    (t (br-to-from-viewer)
	       (error "(br-feature): Cannot find definition of: `%s'" feature-sig))))))

(defun br-features (arg)
  "List features/elements of the current class (prefix ARG = 1) or of the current listing if ARG is other than 0 or 1.

With ARG = 0, the value of the variable, `br-inherited-features-flag', is
toggled and no other action is taken.

If `br-inherited-features-flag' is t, all features of each class are shown.
If nil, only lexically included features are shown and if the features of a
single class are requested and none are defined, the class definition is
displayed so that its feature declarations may be browsed."
  (interactive "p")
  (cond ((and (integerp arg) (= arg 0))
	 (setq br-inherited-features-flag
	       (not br-inherited-features-flag))
	 (message "Inherited features/elements will %sbe shown."
		  (if br-inherited-features-flag "" "not ")))
	(br-inherited-features-flag
	 (br-inherited-features arg))
	(t (br-lexical-features arg))))

;; Used outside of the browser user interface to display classes and features.
(defun br-find ()
  "Prompt with completion for a class or element name from the current Environment and display its definition for editing."
  (interactive)
  (br-edit-entry t))

(defun br-help (&optional file)
  "Display OO-Browser operation help information in the viewer window."
  (interactive)
  (or file (setq file "br-help"))
  (br-file-to-viewer file)
  (save-window-excursion
    (br-to-view-window)
    (br-mode)
    (use-local-map nil))
  (message ""))

(defun br-help-ms ()
  "Display OO-Browser mouse usage help information in the viewer window."
  (interactive)
  (br-help "br-help-ms"))

(defun br-implementors (&optional arg)
  "Display a list of classes which contain definitions for the current element name.
Ignore classes which inherit such definitions.  With optional prefix ARG,
display implementors of all elements within the current listing."
  (interactive "P")
  (let* ((interfaces-p (br-interface-support-p))
	 (categories-p (string-equal br-lang-prefix "objc-"))
	 (feature-regexp (concat "\\`" br-feature-type-regexp " "))
	 (class)
	 (tag)
	 (entries (if arg (br-this-level-entries)
		    (cond ((and (br-at-feature-p)
				(setq tag (br-feature-get-tag)
				      class (br-feature-tag-class tag))
				(not (br-member class '("[module]" "[package]"))))
			   (list (br-find-feature-entry)))
			  ((or (br-at-protocol-p) (br-at-class-category-p))
			   (br-find-class-name-as-list))
			  ;; ignore classes, packages and modules
			  (t nil)))))
    (if (or (null entries) (null (car entries)))
	(error
	 "(OO-Browser):  Apply `br-implementors' to a feature%s%s."
	 (if interfaces-p " or an interface/protocol" "")
	 (if categories-p " or a category" ""))
      (message "Computing implementors...")
      (br-setup-next-window "I")
      (let ((buffer-read-only) (implementor-tags) (classes)
	    (categories)
	    entry-category
	    class
	    start)
	(widen)
	(erase-buffer)
	(mapcar (function
		 (lambda (entry)
		   (cond
		    ;; features
		    ((string-match feature-regexp entry)
		     (setq implementor-tags
			   (br-feature-implementors
			    (br-feature-name entry)))
		     ;; Might contain invalid matches from a default class
		     ;; like [constant] (#define entry), so eliminate any
		     ;; such matches.
		     (if (br-member br-lang-prefix '("c++-" "java-" "objc-" "python-"))
			 (setq implementor-tags
			       (delq nil
				     (mapcar
				      (function
				       (lambda (tag)
					 (setq class (br-feature-tag-class tag))
					 (if (and (br-default-class-p
						   (br-feature-tag-class
						   tag))
						  (not (string-equal class "[function]")))
					     nil
					   tag)))
				      implementor-tags))))
		     ;; Sort tags
		     (setq implementor-tags
			   (br-feature-tag-sort-list implementor-tags))
		     ;; Get classes from tags
		     (setq classes (mapcar 'br-feature-tag-class
					   implementor-tags))
		     (if (equal (string-match br-feature-type-regexp entry) 0)
			 (insert (substring entry 0 2)
				 (br-feature-name entry)
				 "\n")
		       (insert entry "\n"))
		     (setq start (point))
		     (br-insert-classes classes 4)
		     (save-excursion
		       (goto-char start)
		       (br-feature-put-tags implementor-tags)))
		    ;;
		    ;; interfaces/protocols
		    ((and interfaces-p (eq (aref entry 0) ?\<))
		     (br-insert-protocol-implementors
		      (list entry) 0))
		    ;;
		    ;; Objective-C class categories
		    ((setq entry-category (br-class-category-p entry))
		     (if (null categories)
			 (setq categories
			       (mapcar 'br-feature-signature-to-name
				       (br-list-features
					objc-default-category-class))
			       ;; Set to t if there are no categories so that
			       ;; we don't recompute the set of categories
			       ;; for each entry.
			       categories (or categories t)))
		     (setq classes (objc-list-category-classes
				    entry-category categories))
		     (insert entry-category "\n")
		     (setq start (point))
		     (br-insert-classes classes 2))
		    ;;
		    ;; ignore other kinds of entries
		    (t))))
		entries))
      (goto-char 1)
      (message "Computing implementors...Done"))))

(defun br-info-language-specific ()
  "Display the OO-Browser manual section of specifics for the language of the current Environment."
  (interactive)
  (let ((lang-name (cdr (assoc br-lang-prefix br-env-lang-name-alist))))
    (if lang-name
	(id-info (concat "(oo-browser)"
			 lang-name " Specifics"))
      (error "(OO-Browser):  Invalid language prefix, `%s'" br-lang-prefix))))

(defun br-inherited-attributes (arg)
  "Display attributes declared within a class, including those from ancestors.
With optional prefix ARG, display attributes of all classes in the current
listing."
  (interactive "p")
  (let ((br-ancestor-function
	 (function
	  (lambda (class repeated-class indent)
	    (if repeated-class
		nil
	      (br-insert-features
	       (br-feature-list-attributes class) indent))))))
    (br-ancestors arg "attributes" nil)))

(defun br-inherited-features (arg)
  "Display class features, including those from ancestors.
With optional prefix ARG, display features of all classes in the current
listing."
  (interactive "p")
  (let ((br-ancestor-function
	 (function
	  (lambda (class repeated-class indent)
	    (if repeated-class
		nil
	      (br-insert-features (br-list-features class indent) indent))))))
    ;; Features of abstract classes are implemented by their
    ;; descendants so we don't want to show these feature
    ;; names a second time within the inheritance lattice.  The third
    ;; argument to this next call suppresses listing of inherited abstract
    ;; classes and their features except in cases where the class itself is
    ;; abstract.
    (br-ancestors arg "features" t)))

(defun br-inherited-routines (arg)
  "Display class routines, including those from ancestors.
With optional prefix ARG, display routines of all classes in the current
listing."
  (interactive "p")
  (let ((br-ancestor-function
	 (function
	  (lambda (class repeated-class indent)
	    (if repeated-class
		nil
	      (br-insert-features
	       (br-feature-list-routines class) indent))))))
    ;; Routines of abstract classes are implemented by their
    ;; descendants so we don't want to show these routine
    ;; names a second time within the inheritance lattice.  The third
    ;; argument to this next call suppresses listing of inherited abstract
    ;; classes and their routines except in cases where the class itself is
    ;; abstract.
    (br-ancestors arg "routines" t)))

(defun br-kill ()
  "Kill buffer in the viewer window and redisplay help text."
  (interactive)
  (br-do-in-view-window '(progn (kill-buffer nil) (br-help))))

(defun br-lexical-attributes (arg)
  "Display class attributes lexically defined within current class.
With numeric prefix ARG, display attributes of all classes in the current
listing."
  (interactive "p")
  (let ((has-attributes)
	class-list attribute-list class-and-attributes)
    (setq class-list (cond ((and (integerp arg) (/= arg 1))
			    (message "Computing class attributes...")
			    (br-this-level-classes))
			   (t 
			    (br-find-class-name-as-list)))
	  attribute-list
	  (delq nil (mapcar
		     (function
		      (lambda (class)
			(setq class-and-attributes
			      (br-feature-list-attributes class)
			      has-attributes (or has-attributes
					       class-and-attributes))
			(cons class class-and-attributes)))
		     class-list)))
    (cond ((not class-list)
	   (beep)
	   (message "(OO-Browser):  Apply `br-attributes' to a class."))
	  ((not has-attributes)
	   (message "No attributes declared for the class%s."
		    (if (= (length class-list) 1) "" "es"))
	   (beep))
	  (t
	   (br-add-level-hist)
	   (br-next-buffer)
	   (let (buffer-read-only done-set class)
	     (mapcar
	      (function
	       (lambda (class-and-attributes)
		 (setq class (car class-and-attributes))
		 (if (not (br-set-cons done-set class))
		     (insert class " ...\n")
		   ;; Class successfully added to set, so it has not been
		   ;; listed before.
		   (insert class "\n")
		   (br-insert-features (cdr class-and-attributes) 2))))
	      attribute-list)
	     (message "Computing class attributes...Done")
	     (goto-char (point-min)))))))

(defun br-lexical-features (arg)
  "Display class features lexically defined within current class.
With numeric prefix ARG, display features of all classes in the current
listing.

If the features of a single class are requested and there are no feature
definitions for the class, display the class definition so that its feature
declarations may be browsed."
  (interactive "p")
  (let ((has-features)
	class-list feature-list class-and-features)
    (setq class-list (cond ((and (integerp arg) (/= arg 1))
			    (message "Computing class features...")
			    (br-this-level-classes))
			   (t 
			    (br-find-class-name-as-list)))
	  feature-list
	  (delq nil (mapcar
		     (function
		      (lambda (class)
			(setq class-and-features (br-list-features class)
			      has-features (or has-features
					       class-and-features))
			(cons class class-and-features)))
		     class-list)))
    (cond ((not class-list)
	   (beep)
	   (message "(OO-Browser):  Apply `br-features' to a class."))
	  ((not has-features)
	   (if (and (= (length class-list) 1)
		    (br-class-path (car class-list)))
	       (if (br-view nil nil (car class-list))
		   (message
		    "No feature definitions, browse declarations instead."))
	     (message "No class features.") (beep)))
	  (t
	   (br-add-level-hist)
	   (br-next-buffer)
	   (let (buffer-read-only done-set class)
	     (mapcar
	      (function
	       (lambda (class-and-features)
		 (setq class (car class-and-features))
		 (if (not (br-set-cons done-set class))
		     (insert class " ...\n")
		   ;; Class successfully added to set, so it has not been
		   ;; listed before.
		   (insert class "\n")
		   (br-insert-features (cdr class-and-features) 2))))
	      feature-list)
	     (message "Computing class features...Done")
	     (goto-char (point-min)))))))

(defun br-lexical-routines (arg)
  "Display class routines lexically defined within current class.
With numeric prefix ARG, display routines of all classes in the current
listing.

If the routines of a single class are requested and there are no routine
definitions for the class, display the class definition so that its routine
declarations may be browsed."
  (interactive "p")
  (let ((has-routines)
	class-list routine-list class-and-routines)
    (setq class-list (cond ((and (integerp arg) (/= arg 1))
			    (message "Computing class routines...")
			    (br-this-level-classes))
			   (t 
			    (br-find-class-name-as-list)))
	  routine-list
	  (delq nil (mapcar
		     (function
		      (lambda (class)
			(setq class-and-routines
			      (br-feature-list-routines class)
			      has-routines (or has-routines
					       class-and-routines))
			(cons class class-and-routines)))
		     class-list)))
    (cond ((not class-list)
	   (beep)
	   (message "(OO-Browser):  Apply `br-routines' to a class."))
	  ((not has-routines)
	   (if (and (= (length class-list) 1)
		    (br-class-path (car class-list)))
	       (if (br-view nil nil (car class-list))
		   (message
		    "No routine definitions, browse declarations instead."))
	     (message "No class routines.") (beep)))
	  (t
	   (br-add-level-hist)
	   (br-next-buffer)
	   (let (buffer-read-only done-set class)
	     (mapcar
	      (function
	       (lambda (class-and-routines)
		 (setq class (car class-and-routines))
		 (if (not (br-set-cons done-set class))
		     (insert class " ...\n")
		   ;; Class successfully added to set, so it has not been
		   ;; listed before.
		   (insert class "\n")
		   (br-insert-features (cdr class-and-routines) 2))))
	      routine-list)
	     (message "Computing class routines...Done")
	     (goto-char (point-min)))))))

(defun br-lib-rebuild ()
  "Rescan Library components of the current Environment."
  (interactive)
  (if (call-interactively 'br-build-lib-htable)
      (br-show-all-classes)))

(defun br-lib-top-classes (&optional arg)
  "Display a list of the top-level Library classes.
With prefix ARG, display all Library classes."
  (interactive "P")
  (and (or (not (interactive-p))
	   (br-in-top-buffer-p)
	   (y-or-n-p "Exit to top-level class listing buffer? "))
       (cond (arg
	      (br-show-classes
	       (function (lambda () (br-all-classes "lib")))
	       nil t "Al")
	      (message "Listing of all Library classes"))
	     (t
	      (br-show-classes 'br-get-lib-top-classes t t "Tl")
	      (message "Listing of top-level Library classes")))
       (setq *br-level-hist* nil)))

(defun br-match (&optional expr arg again matched)
  "Show all class names in the current Environment that contain optional EXPR.
A nil value of EXPR means prompt for a value.  With optional prefix ARG, EXPR
is treated as a string.  By default, it is treated as a regular expression.
AGAIN non-nil shows the number of classes MATCHED from the last search,
allowing repeated narrowing of the search set.  An empty EXPR when AGAIN is
nil matches to all classes in the Environment."
  (interactive (list nil current-prefix-arg))
  (or expr (setq expr (read-string
		       (concat (if again (format "(%s matches)  " matched))
			       (if arg
				   "Find Environment class string matches"
				 "Find Environment class regular expression matches")
			       (if again " (RET to end): " ": ")))))
  (if (and again (equal expr ""))
      nil
    (let* ((match-expr (if arg (regexp-quote expr) expr))
	   (classes
	    (delq nil (mapcar
		       (function
			(lambda (cl)
			  (if (string-match match-expr cl) cl)))
		       (if again
			   (sort (br-this-level-classes) 'string-lessp)
			 (br-all-classes))))))
      (if classes
	  (progn (let (buffer-read-only)
		   (br-feature-clear-tags)
		   (erase-buffer)
		   (br-insert-classes classes 0))
		 (goto-char (point-min))
		 (br-match nil arg t (br-count)))
	(beep)
	(message "No matches for \"%s\"." expr)))))

(defun br-match-entries (&optional expr arg again matched)
  "Show all entries in the current listing that contain optional EXPR.
A nil value of EXPR means prompt for a value.  With optional prefix ARG, EXPR
is treated as a string.  By default, it is treated as a regular expression.
AGAIN non-nil means show the number of entries MATCHED from the last search,
allowing repeated narrowing of the search set.  An empty EXPR when AGAIN is
nil matches to all entries in the listing."
  (interactive (list nil current-prefix-arg))
  (or expr (setq expr (read-string
			(concat (if again (format "(%s matches)  " matched))
				(if arg
				    "Find string matches in listing"
				  "Find regular expression matches in listing")
				(if again " (RET to end): " ": ")))))
  (if (and again (equal expr ""))
      nil
    (let* ((match-expr (if arg (regexp-quote expr) expr))
	   (buffer-read-only))
      (goto-char (point-min))
      (if (not (re-search-forward match-expr nil t))
	  (progn (beep)
		 (message "No matches for \"%s\"." expr))
	(goto-char (point-min))
	(delete-non-matching-lines match-expr)
	(goto-char (point-min))
	(br-match-entries nil arg t (br-count))))))

(defun br-next-entry (arg)
  "Move point vertically down prefix ARG number of lines in a listing buffer."
  (interactive "p")
  (let ((end))
    (setq end (= (forward-line arg) arg))
    (and (looking-at "^$") (forward-line -1) (setq end t))
    (and end (message "No next entry.") (beep))))

(defun br-order (arg)
  "Order current browser listing window entries.
With prefix ARG other than 1 (the default), don't remove leading space from
entry lines before ordering.  Negative ARG means order in descending Ascii
sequence, otherwise order in ascending sequence."
  (interactive "p")
  (setq arg (or arg 1))
  (message "Ordering entries...")
  (let ((buffer-read-only)
	sort-args)
    (and (= arg 1) (progn (goto-char (point-min))
			  (while (re-search-forward "^[ \t]+" nil t)
			    (replace-match ""))))
    (if (string-lessp "19" emacs-version)
	(progn
	  ;; Emacs 19: This is slower than calling an external sort but it
	  ;; maintains the element tags in a listing, allowing further
	  ;; browsing from this buffer.
	  (sort-lines (< arg 0) (point-min) (point-max))
	  ;; Move [default] classes to the end of the sorted list.
	  (goto-char (point-min))
	  (if (re-search-forward "^[ \t]*\\[" nil t)
	      (let (start end)
		(beginning-of-line)
		(setq start (point))
		(goto-char (point-max))
		(re-search-backward "^[ \t]*\\[" nil t)
		(forward-line 1)
		(setq end (point))
		(goto-char (point-max))
		(append-to-buffer (current-buffer) start end)
		(delete-region start end))))
      ;;
      ;; Emacs 18: We can't maintain the buffer tags, so we just use a fast
      ;; external sort.
      (setq sort-args (list (point-min) (point-max) "sort" t t nil)
	    sort-args (if (< arg 0)
			  (if (stringp br-sort-options)
			      (nconc sort-args (list "-r" br-sort-options))
			    (nconc sort-args (list "-r")))
			(if (stringp br-sort-options)
			    (nconc sort-args (list br-sort-options))
			  sort-args)))
      (apply 'call-process-region sort-args)))
  (goto-char (point-min))
  (message "Ordering entries...Done"))

(defun br-parents (&optional arg)
  "Display the parents of the current class.
With optional prefix ARG, display parents of all the classes in the current
listing."
  (interactive "P")
  (let ((class-list (cond (arg
			   (message "Computing parents...")
			   (br-this-level-classes))
			  (t
			   (br-find-class-name-as-list))))
	(has-parents)
	parents parents-list)
    (setq parents-list
	  (delq nil (mapcar (function
			     (lambda (class)
			       (setq parents (br-get-parents class)
				     has-parents (or has-parents parents))
			       (cons class parents)))
			    class-list)))
    (cond ((not parents-list)
	   (message "(OO-Browser):  Apply `br-parents' to a class.") (beep))
	  ((not has-parents)
	   (message "No parents.") (beep))
	  (t
	   (br-setup-next-window "p")
	   (let (buffer-read-only done-set class)
	     (mapcar
	      (function
	       (lambda (class-parents-cons)
		 (setq class (car class-parents-cons))
		 (if (not (br-set-cons done-set class))
		     (insert class " ...\n")
		   ;; Class successfully added to set, so it has not been
		   ;; listed before.
		   (insert class "\n")
		   (br-insert-classes (cdr class-parents-cons) 2))))
	      parents-list))
	   (if arg (message "Computing parents...Done"))
	   (goto-char (point-min))
	   t))))

(defun br-prev-entry (arg)
  "Move point vertically up prefix ARG number of lines in a listing buffer."
  (interactive "p")
  (setq arg (- arg))
  (and (= (forward-line arg) arg)
       (message "No previous entry.")
       (beep)))

(defalias 'br-interfaces 'br-protocols)

(defun br-protocols (&optional arg)
  "Display the protocols to which the current class or protocol conforms, including inherited ones.

With optional prefix ARG (other than 0 or 1), display protocols of all
classes and protocols in the current listing.

With ARG = 0, the value of the variable, `br-protocols-with-classes-flag', is
toggled and no other action is taken."
  (interactive "P")
  (if (and (integerp arg) (= arg 0)
	   (br-protocol-support-p))
      (progn
	(setq br-protocols-with-classes-flag
	      (not br-protocols-with-classes-flag))
	(if br-protocols-with-classes-flag
	    (message
	     "Protocols/interfaces will %sbe included in initial class listings."
	     (if br-protocols-with-classes-flag "" "not "))))
    (let* ((protocols-p (br-protocol-support-p))
	   (class-list (if protocols-p
			   (cond (arg
				  (message "Computing all class protocols...")
				  (br-this-level-classes))
				 (t 
				  (br-find-class-name-as-list))))))
      (cond ((not protocols-p)
	     (beep)
	     (message "(OO-Browser):  No protocol browsing support for this language"))
	    ((not class-list)
	     (beep)
	     (message "(OO-Browser):  Apply `br-protocols' to a class."))
	    (t
	     (br-setup-next-window "P")
	     (let (buffer-read-only)
	       (br-ancestor-trees class-list))
	     (message "Computing all class protocols...Done")
	     (goto-char (point-min)))))))

(defun br-quit (&optional arg)
  "Quit the OO-Browser.
With optional prefix ARG, delete window configurations and listing
buffers associated with the browser."
  (interactive "P")
  (if (not (br-in-browser))
      (br-interrupt arg)
    (if (null arg)
	(setq *br-save-wconfig* (current-window-configuration))
      (if (featurep 'br-tree) (br-tree-kill))
      (br-viewer-kill)
      ;; Too dangerous to include (br-editor-kill) here.
      ;; The user can invoke it manually if desired.
      )
    ;; The following `let' clause is necessary since br-interrupt buries
    ;; buffers and so must be called before the window configuration restore,
    ;; but it also may set *br-prev-wconfig* to nil, so we have to cache its
    ;; value.
    (let ((wconfig *br-prev-wconfig*))
      (br-interrupt arg)
      (if wconfig (br-set-window-configuration wconfig)))
    (and (fboundp 'frame-name) (equal (frame-name) "OO-Browser")
	 (if arg (delete-frame) (make-frame-invisible)))
    nil)
  ;; Force menubar update under GNU Emacs.
  (if (fboundp 'set-menubar-dirty-flag)
      (set-menubar-dirty-flag)))

(defun br-refresh ()
  "Restore the OO-Browser to its state upon startup."
  (interactive)
  (br-window-setup)
  (br-show-all-classes)
  (br-help)
  (setq br-in-browser (selected-frame)))

(defun br-report-bug ()
  "Send a message to the OO-Browser discussion list."
  (interactive)
  (if (br-in-browser) (br-to-view-window))
  (hmail:compose "oo-browser@beopen.com" '(hypb:configuration)))

(defun br-routines (arg)
  "Display routines of the current class (prefix ARG = 1) or of the current listing if ARG is other than 0 or 1.

With ARG = 0, the value of the variable, `br-inherited-features-flag', is
toggled and no other action is taken.

If `br-inherited-features-flag' is t, all routines of each class are shown.
If nil, only lexically included routines are shown and if the routines of a
single class are requested and none are defined, the class definition is
displayed so that its routine declarations may be browsed."
  (interactive "p")
  (cond ((and (integerp arg) (= arg 0))
	 (setq br-inherited-features-flag
	       (not br-inherited-features-flag))
	 (message "Inherited features/elements will %sbe shown."
		  (if br-inherited-features-flag "" "not ")))
	(br-inherited-features-flag
	 (br-inherited-routines arg))
	(t (br-lexical-routines arg))))

(defun br-set-window-configuration (wconfig)
  "Restore a window configuration, if possible, protecting against errors.
Return t if no error, nil otherwise."
  (condition-case ()
      (progn (set-window-configuration wconfig) t)
    (error nil)))

(defun br-sys-rebuild ()
  "Rescan System components of the current Environment."
  (interactive)
  (if (call-interactively 'br-build-sys-htable)
      (br-show-all-classes)))

(defun br-sys-top-classes (&optional arg)
  "Display list of top-level System classes.
With prefix ARG, display all System classes."
  (interactive "P")
  (and (or (not (interactive-p))
	   (br-in-top-buffer-p)
	   (y-or-n-p "Exit to top-level class listing buffer? "))
       (cond (arg
	      (br-show-classes
	       (function (lambda () (br-all-classes "sys")))
	       nil t "As")
	      (message "Listing of all System classes"))
	     (t
	      (br-show-classes 'br-get-sys-top-classes t t "Ts")
	      (message "Listing of top-level System classes")))
       (setq *br-level-hist* nil)))

;;;###autoload
(defun br-to-from-viewer ()
  "Move point to the viewer window or back to the last recorded listing window."
  (interactive)
  (if (br-in-view-window-p)
      (progn (if (and *br-prev-listing-window*
		      (if (fboundp 'window-live-p)
			  (window-live-p *br-prev-listing-window*)
			t))
		 (select-window *br-prev-listing-window*)
	       (other-window 1))
	     (setq *br-prev-listing-window* nil))
    (br-to-view-window)))

(defun br-toggle-c-tags ()
  "Toggle the value of the `br-c-tags-flag' flag."
  (interactive)
  (setq br-c-tags-flag (not br-c-tags-flag))
  (message "C constructs will %sbe added to C-based language Environments."
	   (if br-c-tags-flag "" "not ")))

(defun br-toggle-keep-viewed ()
  "Toggle the value of the `br-keep-viewed-classes' flag."
  (interactive)
  (setq br-keep-viewed-classes (not br-keep-viewed-classes))
  (message "Viewed buffers will no%s be kept after use."
	   (if br-keep-viewed-classes "w" "t")))

(defun br-show-all-classes ()
  "Display list of all Environment classes."
  (interactive)
  (br-show-top-classes t))

(defun br-show-top-classes (&optional arg)
  "Display list of top-level classes.
With prefix ARG, display all Environment classes."
  (interactive "P")
  (and (or (not (interactive-p))
	   (br-in-top-buffer-p)
	   (y-or-n-p "Exit to top-level class listing buffer? "))
       (cond (arg
	      (br-show-classes 'br-all-classes nil t "A")
	      (message "Listing of all Environment classes"))
	     (t
	      (br-show-classes 'br-get-top-classes t t "T")
	      (message "Listing of top-level classes")))
       (setq *br-level-hist* nil)))

(defun br-unique ()
  "Eliminate adjacent duplicate entry names from the current listing window.
If two adjacent entries look the same, one is eliminated, even if they refer
to different class elements."
  (interactive)
  (let ((buffer-read-only)
	(again t)
	first second)
    (goto-char (point-min))
    (setq first (br-feature-current))
    (while again
      (setq again (= (forward-line 1) 0)
	    second (br-feature-current))
      (if (not (string-equal first second))
	  (setq first second)
	(beginning-of-line)
	(delete-region (point) (progn (forward-line 1) (point)))
	;; back up to first line again
	(forward-line -1)))
    (goto-char (point-min))))

(defun br-version ()
  "Display the OO-Browser version number and credits."
  (interactive)
  (br-funcall-in-view-window
   (concat br-buffer-prefix-info "Help*")
   (function (lambda ()
	       (insert-file-contents (br-pathname "BR-VERSION"))
	       (hypb:display-file-with-logo)
	       (if (re-search-forward "<VERSION>" nil t)
		   (replace-match br-version t t))
	       (center-line)
	       (set-buffer-modified-p nil)))))

(defun br-view-entry (&optional prompt)
  "Display source for any browser listing entry.
Optional prefix arg PROMPT (if other than 0) means prompt for an entry name;
automatically prompts if called interactively outside of a listing window,
e.g. within a source code buffer when the browser user interface is not
displayed.  With a prefix arg of 0, the value of the variable,
`br-keep-viewed-classes', is toggled and no other action is taken."
  (interactive "P")
  (if (and (integerp prompt) (= prompt 0))
      (br-toggle-keep-viewed)
    (let ((entry) (feature-tag))
      (if (or prompt (and (interactive-p) (not (br-in-browser))))
	  (cond ((and (setq entry (br-complete-entry))
		      (string-match br-feature-signature-regexp entry))
		 (if (setq feature-tag (car (br-feature-tag-and-file entry)))
		     (br-feature 'view feature-tag)
		   (error "(br-feature-tag-and-file): Could not find match for: `%s'" entry)))
		(entry ;; class name
		 (br-view nil nil entry))
		(t (error "(br-complete-entry): Exited without selecting a match")))
	(cond ((br-at-feature-p)
	       (br-feature 'view)
	       t)
	      ((and (setq entry (br-find-class-name))
		    (br-class-in-table-p entry))
	       (br-view nil nil entry))
	      (t (error "(OO-Browser):  Entry may be referenced but not defined in the Environment.")))))))

(defun br-view (&optional prompt writable class)
  "Displays class file in viewer window.
Optional prefix arg PROMPT means prompt for class name.  Non-nil WRITABLE means
allow editing, otherwise display in read-only mode.  Non-nil CLASS is class to
display.  Return t if class is displayed or sent to an external viewer, else nil."
  (interactive "P")
  (or class (setq class (if prompt (br-complete-class-name)
			  (br-find-class-name))))
  (cond ((null class)
	 (beep)
	 (message "(OO-Browser):  Select a class to view.")
	 nil)
	((not (br-class-defined-p class)) nil)
	((and (or (and writable (br-edit-externally-p))
		  (and (not writable) (br-view-externally-p)))
	      (let* ((func (function
			    (lambda (path unused)
			      (set-buffer (find-file-noselect path)))))
		     (br-edit-file-function func)
		     (br-view-file-function func)
		     (line-num (save-excursion
				 (br-find-class class (not writable)))))
		(and line-num
		     (funcall (if writable 'br-edit-externally 'br-view-externally)
			      (br-class-path class) line-num)))))
	(t (let ((owind (selected-window))
		 viewer-obuf)
	     (unwind-protect
		 (progn (if (br-in-browser) (br-to-view-window))
			(setq viewer-obuf (current-buffer))
			(if (br-find-class class (not writable))
			    (progn 
			      (if (not (eq (current-buffer) viewer-obuf))
				  (save-excursion
				    (set-buffer viewer-obuf)
				    (if (and (not br-keep-viewed-classes)
					     buffer-read-only
					     (null (buffer-modified-p)))
					(kill-buffer (current-buffer)))))
			      t)))
	       (or writable (select-window owind)))))))

(defun br-view-ext (viewer-cmd file line-num)
  "Invoke a non-standard VIEWER-CMD on FILE at LINE-NUM.
See also `br-viewer-cmd'."
  (interactive "fFile to view: ")
  (or viewer-cmd (setq viewer-cmd br-viewer-cmd))
  (if (not (stringp viewer-cmd))
      ;; must be a Lisp function that takes two args, a file and line number
      (funcall viewer-cmd file line-num)
    (setq delete-exited-processes t)
    (let ((proc)
	  (name (concat br-vw-name (int-to-string br-vw-num))))
      (setq br-vw-num (1+ br-vw-num)
	    proc (br-view-ext-start viewer-cmd name file line-num))
      (if proc
	  (process-kill-without-query proc)
	(beep)
	(message "(OO-Browser):  Could not start external view process: %s"
		  viewer-cmd)))))

(defun br-view-full-frame ()
  "Delete all windows in the selected frame except for the viewer window."
  (interactive)
  (setq *br-save-wconfig* (current-window-configuration))
  (br-to-view-window)
  (let ((buf (current-buffer)))
    (br-interrupt)
    (delete-other-windows)
    (switch-to-buffer buf))
  (let* ((cmd (concat br-lang-prefix "browse"))
	 (key (car (where-is-internal (intern-soft cmd)))))
    (message "Recall OO-Browser with: {%s}"
	     (if key
		 (key-description key)
	       (concat (key-description
			(or (car (where-is-internal
				  'execute-extended-command))
			    "\M-x"))
		       " " cmd)))))

(defun br-viewer-scroll-down-by-line (arg)
  "Scroll the viewer window from within a listing window to show prefix ARG more prior lines (default is 1)."
  (interactive "p")
  (let ((owind (selected-window)))
    (unwind-protect
	(progn (br-to-view-window)
	       (scroll-down arg))
      (select-window owind))))

(defun br-viewer-scroll-up-by-line (arg)
  "Scroll the viewer window from within a listing window to show prefix ARG more following lines (default is 1)."
  (interactive "p")
  (let ((owind (selected-window)))
    (unwind-protect
	(progn (br-to-view-window)
	       (scroll-up arg))
      (select-window owind))))

(defun br-viewer-beginning-of-buffer ()
  "Scroll to the beginning of the viewer window buffer from within a listing window."
  (interactive)
  (let ((owind (selected-window)))
    (br-to-view-window)
    (beginning-of-buffer) ;; sets mark at prior location
    (select-window owind))
  (message "Beginning of buffer"))

(defun br-viewer-end-of-buffer ()
  "Scroll to the end of the viewer window buffer from within a listing window."
  (interactive)
  (let ((owind (selected-window)))
    (br-to-view-window)
    (end-of-buffer) ;; sets mark at prior location
    (select-window owind))
  (message "End of buffer"))

(defun br-viewer-kill ()
  "Kill all current external viewer sub-processes."
  (interactive)
  (if (br-kill-process-group br-vw-name br-vw-num "external viewers")
      (setq br-vw-num 0)))

(defun br-viewer-scroll-down (&optional arg)
  "Scroll the viewer window downward ARG lines or a windowful if no ARG."
  (interactive "P")
  (let ((owind (selected-window)))
    (unwind-protect
	(progn (br-to-view-window)
	       (scroll-down arg))
      (select-window owind))))

(defun br-viewer-scroll-up (&optional arg)
  "Scroll the viewer window upward ARG lines or a windowful if no ARG."
  (interactive "P")
  (let ((owind (selected-window)))
    (unwind-protect
	(progn (br-to-view-window)
	       (scroll-up arg))
      (select-window owind))))

(defun br-where (&optional prompt)
  "Display in the viewer window and return the full path of the defining file for a browser listing entry.
Optional prefix arg PROMPT means prompt for the entry name; automatically
prompts if called interactively outside of a listing window (in standalone
mode), e.g. within a source code buffer when the browser user interface is
not displayed.  If called in standalone mode with a prefix argument, the
command inserts the defining path at point rather than displaying it elsewhere." 
  (interactive "P")
  (let ((entry) (path) (tag)
	(standalone (and (interactive-p) (not (br-in-browser)))))
    (if (or prompt standalone)
	(cond ((and (setq entry (br-complete-entry))
		    (string-match br-feature-signature-regexp entry))
	       (setq path (cdr (br-feature-tag-and-file entry))))
	      (entry ;; class name
	       (setq path (br-class-defined-p entry)))
	      (t (error "(br-complete-entry): Exited without selecting a match")))
      (cond ((setq tag (br-feature-get-tag))
	     (setq path (br-feature-tag-path tag))
	     (if (equal br-lang-prefix "eif-")
		 (setq entry (br-feature-tag-name tag t))
	       (setq entry (br-feature-tag-signature tag))
	       (if (and entry (string-match "::" entry))
		   nil
		 (setq entry (concat (br-feature-tag-class tag) "::" entry)))
	       ;; Remove any trailing \{.
	       (if (string-match "\\s-*\{\\'" entry)
		   (setq entry (substring entry 0 (match-beginning 0))))))
	    ((setq entry (br-find-class-name))
	     (or (setq path (br-class-path entry))
		 (error "(OO-Browser):  No path for this class in current Environment")))
	    (t (error "(OO-Browser):  No entry for current line in current Environment"))))
    (cond ((null path) nil)
	  ((and prompt standalone) (insert path))
	  (t (let ((owind (selected-window))
		   (buf (buffer-name)))
	       (br-to-view-window)
	       (switch-to-buffer (get-buffer-create (concat buf "-Path")))
	       (setq buffer-read-only nil)
	       (buffer-disable-undo (current-buffer))
	       (erase-buffer)
	       (insert (format "`%s' is defined within\n    \"%s\"" entry path))
	       (br-major-mode)
	       (goto-char 1)
	       (select-window owind)
	       (message ""))))
    path))

(defun br-write-buffer (file)
  "Write the narrowed portion of the current browser buffer to a file."
  (interactive "FFile to write buffer to: ")
  (write-region (point-min) (point-max) file))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun br-add-level-hist ()
  ;; Even though this next line looks useless, it cures a problem with
  ;; window buffer correspondences when the OO-Browser is started, so don't
  ;; remove it.
  (set-buffer (window-buffer (selected-window)))
  (setq *br-level-hist*
	(cons (list (selected-window) (buffer-name) (br-wind-line-at-point))
	      *br-level-hist*)))

(defun br-ancestor-roots (class-list)
  "Return sorted list of CLASS-LIST's unique ancestors which do not inherit from any other class.
This list may include elements from CLASS-LIST itself."
  (let ((rtn) (parents) func)
    (setq func (function
		(lambda (class-list)
		  (mapcar
		   (function
		    (lambda (class)
		      (if (not (setq parents (br-get-parents class)))
			  (setq rtn (cons class rtn))
			(funcall func parents))))
		   class-list))))
    (funcall func class-list)
    (br-set-of-strings (sort rtn 'string-lessp))))

(defun br-ancestor-trees (class-list &optional depth offset concrete-classes-flag)
  "Insert ancestor trees starting with classes from CLASS-LIST.
Ancestor trees are not inverted, i.e. parents appear below children, not
above.  Indent each class in CLASS-LIST by optional DEPTH spaces (default is
0 in order to ensure proper initialization).  Offset each child level by
optional OFFSET spaces from its parent (which must be greater than zero,
default 2).  CONCRETE-CLASSES-FLAG non-nil means omit abstract classes from
the trees."
  (or offset (setq offset 2))
  (or depth (setq depth 0))
  (if (= depth 0) (setq br-tmp-class-set nil))
  (let ((prev-expansion-str " ...")
	parents expand-subtree)
    (mapcar
      (function
	(lambda (class)
	  (setq expand-subtree (br-set-cons br-tmp-class-set class)
		parents (if expand-subtree (br-get-parents class)))
	  (indent-to depth)
	  (insert class)
	  (and (not expand-subtree) (br-has-parents-p class)
	       (insert prev-expansion-str))
	  (insert "\n")
	  (if br-ancestor-function
	      (funcall br-ancestor-function
		       class (not expand-subtree) (+ depth offset)))
	  (if parents
	      (br-ancestor-trees parents (+ depth offset) offset
				 concrete-classes-flag))))
      (if (and concrete-classes-flag
	       ;; Always include the first level classes
	       (not (zerop depth)))
	  (delq nil (mapcar 'br-concrete-class-p class-list))
	class-list)))
  (if (zerop depth) (setq br-tmp-class-set nil)))

(defun br-ancestor-trees-inverted (class-list &optional depth offset concrete-classes-flag)
  "Insert ancestor trees starting with classes from CLASS-LIST.
Ancestor trees are inverted, parents appear above children as in other
browser listing windows.  Indent each class in CLASS-LIST by optional DEPTH
spaces (default is 0 in order to ensure proper initialization).  Offset each
child level by optional OFFSET spaces from its parent (which must be greater
than zero, default 2).  CONCRETE-CLASSES-FLAG non-nil means omit abstract
classes from the trees."
  (or offset (setq offset 2))
  (or depth (setq depth 0 br-tmp-depth 0))
  (if (= depth 0) (setq br-tmp-class-set nil))
  (let ((prev-expansion-str " ...")
	parents expand-subtree)
    (mapcar (function
	      (lambda (class)
		(setq expand-subtree (br-set-cons br-tmp-class-set class)
		      parents (if expand-subtree (br-get-parents class)))
		(if parents
		    (progn (setq br-tmp-depth
				 (max (+ depth offset) br-tmp-depth))
			   (br-ancestor-trees-inverted
			    parents (+ depth offset) offset
			    concrete-classes-flag)))
		(indent-to (- br-tmp-depth depth))
		(insert class)
		(and (not expand-subtree) (br-has-children-p class)
		     (insert prev-expansion-str))
		(insert "\n")
		(if br-ancestor-function
		    (funcall br-ancestor-function
			     class (not expand-subtree) (+ depth offset)))
		(if (= depth 0) (setq br-tmp-depth 0))))
	    (if (and concrete-classes-flag
		     ;; Always include the first level classes
		     (not (zerop depth)))
		(delq nil (mapcar 'br-concrete-class-p class-list))
	      class-list)))
  (if (zerop depth) (setq br-tmp-class-set nil)))

(defun br-at-class-category-p ()
  "Returns t iff point is on a class category listing line."
  (if (string-equal br-lang-prefix "objc-")
      (save-excursion (beginning-of-line) (looking-at ".*\("))))

(defun br-at-default-class-p ()
  "Returns t iff point is within a default class listing entry."
  (and (save-excursion
	 (beginning-of-line)
	 (looking-at "[ \t]*\\(\\[[^\]]+\\]\\)"))
       (>= (point) (match-beginning 1))
       (< (point) (match-end 1))))

(defun br-at-feature-p ()
  "Returns t iff point is on a feature listing line."
  ;; Sometimes as in the case of implementor entries, the listing entry
  ;; itself does not begin with a feature indicator character but the
  ;; entry from its associated tag does, so use that to test.
  (let* ((tag (br-feature-get-tag))
	 (entry (and tag (br-feature-tag-name tag nil t))))
    (and entry (equal (string-match br-feature-entry-regexp entry) 0))))

(defun br-at-protocol-p ()
  "Return non-nil if point is within a protocol listing entry line."
  (and (br-protocol-support-p)
       (save-excursion (beginning-of-line) (looking-at "[ \t]*<"))))

(defun br-browser-buffer-p (&optional buffer)
  "Returns t iff optional BUFFER or current buffer is an OO-Browser specific buffer."
  (equal 0 (string-match (concat br-buffer-prefix-inher
				 "\\|" br-buffer-prefix-blank
				 "\\|" (regexp-quote br-buffer-prefix-info))
			 (buffer-name buffer))))

(defun br-buffer-level ()
  "Returns current listing buffer level as a string."
  (let* ((name (buffer-name))
	 (pos (string-match "-[^-0-9]*\\([0-9]+\\)\\'" name)))
    (if pos (substring name (match-beginning 1) (match-end 1)))))

(defun br-class-level ()
  "Returns current class hierarchy level as an integer.
1 is the top level."
  (let ((level-string (br-buffer-level)))
    (if level-string (string-to-int level-string))))

(defun br-listing-window-num ()
  "Return listing window number, leftmost is 1, non-listing window = 0."
  (let ((wind (selected-window))
	(ctr 0))
    (br-to-view-window)
    (while (not (eq wind (selected-window)))
      (other-window 1)
      (setq ctr (1+ ctr)))
    ctr))

(defun br-cleanup ()
  "Cleanup and free browser Environment data structures."
  (let ((env-file (intern-soft (concat br-lang-prefix "env-file"))))
    (if env-file (set env-file nil)))
  (setq br-lang-prefix nil
	br-sys-paths-htable nil
	br-lib-paths-htable nil
	br-paths-htable nil
	br-sys-parents-htable nil
	br-lib-parents-htable nil
	br-parents-htable nil
	br-children-htable nil
	br-lib-prev-search-dirs nil
	br-sys-prev-search-dirs nil
	))

(defun br-clear (command-string)
  "Re-initialize all browser listing buffer displays.
COMMAND-STRING is a short mnemonic string to attach to the first listing
buffer name to help describe the listing command used."
  (let ((n (max 1 (/ (frame-width) br-min-width-window))))
    (br-to-view-window)
    (other-window 1)
    ;; This next expression resets the first level buffer name to be more
    ;; descriptive of what is being shown.
    (br-next-buffer (concat command-string "1"))
    (while (> n 1)
      (setq n (1- n))
      (br-next-buffer nil br-buffer-prefix-blank))
    (br-to-view-window)
    (other-window 1)))

(defun br-descendant-trees (class-list &optional indent offset)
  "Insert descendant trees starting with classes from CLASS-LIST.
Indent each class in CLASS-LIST by optional INDENT spaces (default is 0 in
order to ensure proper initialization).  Offset each child level by optional
OFFSET spaces from its parent (which must be greater than zero, default 2)."
  (or indent (setq indent 0))
  (or offset (setq offset 2))
  (if (= indent 0) (setq br-tmp-class-set nil))
  (let ((prev-expansion-str " ...")
	children expand-subtree)
    (mapcar (function
	      (lambda (class)
		(setq expand-subtree (br-set-cons br-tmp-class-set class)
		      children (if expand-subtree (br-get-children class)))
		(indent-to indent)
		(insert class)
		(and (not expand-subtree) (br-has-children-p class)
		     (insert prev-expansion-str))
		(insert "\n")
		(if children
		    (br-descendant-trees children (+ indent offset) offset))))
	    class-list))
  (if (= indent 0) (setq br-tmp-class-set nil)))

(defun br-display-buffer (suffix)
  "Displays browser buffer ending in SUFFIX in current window."
  (let ((buf (get-buffer (concat br-buffer-prefix suffix))))
    (if buf (progn (set-window-buffer (selected-window) buf)))
    buf))

(defun br-do-in-view-window (form)
  "Evaluate FORM in viewer window and then return to current window."
  (interactive)
  (let ((wind (selected-window)))
    (unwind-protect
	(progn (br-to-view-window)
	       (eval form))
      (select-window wind))))

(defun br-edit-ext-start (editor-cmd name file line-num)
  "Start an external viewer given by EDITOR-CMD using NAME applied to FILE at LINE-NUM."
  (apply 'start-process name name editor-cmd
	 (if (equal editor-cmd "xterm")
	     (nconc (list "-title" (if (stringp br-ed2)
				       (concat br-ed2 ": " file)
				     file))
		    (delq nil (list br-ed1 br-ed2
				    (if line-num (format "+%s" line-num))
				    br-ed3 br-ed4 br-ed5 br-ed6 br-ed7 br-ed8
				    br-ed9))
		    (list file))
	   (nconc (delq nil (list br-ed1 br-ed2
				  (if line-num (format "+%s" line-num))
				  br-ed3 br-ed4 br-ed5 br-ed6
				  br-ed7 br-ed8 br-ed9))
		  (list file)))))

(defun br-edit-externally-p ()
  (and br-editor-cmd (or hyperb:window-system
			 ;; Support custom Lisp-based edit commands on any
			 ;; display type.
			 (not (stringp br-editor-cmd)))))

(defun br-view-externally-p ()
  (and br-viewer-cmd (or hyperb:window-system
			 ;; Support custom Lisp-based view commands
			 ;; on any display type.
			 (not (stringp br-viewer-cmd)))))

(defun br-edit-externally (path line-num)
  "Displays PATH at LINE-NUM in a writable fashion using an external program.
If cannot display PATH, returns nil."
  (br-edit-ext br-editor-cmd path line-num))

(defun br-view-externally (path line-num)
  "Displays PATH at LINE-NUM in a read-only fashion using an external program and returns t.
If cannot display PATH, returns nil."
  (br-view-ext br-viewer-cmd path line-num))

(defun br-funcall-in-view-window (buffer function &optional no-erase)
  "Clear out BUFFER and display return value from invocation of FUNCTION in viewer window.
Move point to beginning of buffer and then return to current window.  BUFFER
may be a buffer name.
With optional NO-ERASE, buffer is not erased before function is called."
  (interactive)
  (let ((wind (selected-window)))
    (unwind-protect
	(progn (br-to-view-window)
	       (set-window-buffer (selected-window) (get-buffer-create buffer))
	       (let (buffer-read-only)
		 (if no-erase
		     (goto-char (point-min))
		   (erase-buffer))
		 (funcall function))
	       (goto-char (point-min)))
      (select-window wind))))

(defun br-file-to-viewer (filename)
  "Display FILENAME from OO-Browser source directory in browser viewer window.
FILENAME should not contain any path information."
  (br-funcall-in-view-window
   (concat br-buffer-prefix-info "Help*")
   (function (lambda ()
	       (insert-file-contents (br-pathname filename))
	       (set-buffer-modified-p nil)))))

(defun br-in-browser ()
  "Return selected frame if the OO-Browser is active in it, else return nil."
  (cond ((not (eq br-in-browser (selected-frame))) nil)
	((or (one-window-p 'nomini)
	     (and (fboundp 'window-list)
		  (< (length (window-list)) 3)))
	 (setq br-in-browser nil))
	(t br-in-browser)))


(defun br-in-top-buffer-p ()
  "Return t if point is in the top class listing buffer, else nil."
  (string-equal (br-buffer-level) "1"))

(defun br-in-view-window-p ()
  "Is point in a viewer window?
May return t even if not in the OO-Browser."
  (and (not (eq (selected-window) (minibuffer-window)))
       (br-non-listing-window-p)))

(defun br-init (env-file)
  "Initialization common to all OO-Browser invocations.  Uses ENV-FILE argument."
  (save-excursion (br-feature-tags-init env-file)))

(defun br-insert-classes (class-list &optional indent)
  "Insert CLASS-LIST in current buffer indented INDENT columns."
  (mapcar (function
	    (lambda (class-name)
	      (and indent (indent-to indent))
	      (and class-name (insert class-name "\n"))))
	  class-list))

(defun br-insert-protocol-implementors (protocol-list indent)
  (or indent (setq indent 0))
  (mapcar
   (function
    (lambda (item)
      (if (eq (aref item 0) ?\<)
	  ;; abstract class
	  (progn (if (zerop indent)
		     (progn (indent-to indent) (insert item "\n")))
		 (br-insert-protocol-implementors (br-get-children item)
						  (+ indent 2)))
	;; Don't recurse on non-abstract classes since their children
	;; simply inherit conformance to the protocol from them, so they
	;; do not add any information as implementors.
	(indent-to indent) (insert item "\n"))))
   protocol-list))

(defun br-interrupt (&optional arg)
  (if (null arg)
      (mapcar
       (function
	(lambda (buf)
	  (set-buffer buf)
	  (if (or (eq major-mode 'br-mode) (br-browser-buffer-p))
	      (bury-buffer nil))))
       (buffer-list))
    (setq *br-save-wconfig* nil
	  *br-prev-wconfig* nil
	  *br-prev-listing-window* nil)
    (mapcar
     (function
      (lambda (buf)
	(set-buffer buf)
	(if (or (eq major-mode 'br-mode)
		(br-browser-buffer-p))
	    (progn (let (buffer-read-only)
		     (br-feature-clear-tags)
		     (set-buffer-modified-p nil))
		   (kill-buffer (current-buffer))))))
     (buffer-list))
    (br-cleanup))
  (setq br-in-browser nil))

(defun br-mode ()
  "The major mode used by OO-Browser listing windows.
See the file \"br-help\" for browser usage information.
It provides the following keys: \\{br-mode-map}"
  (interactive)
  (use-local-map br-mode-map)
  (setq major-mode 'br-mode)
  (setq mode-name "OO-Browse")
  (set-syntax-table text-mode-syntax-table)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq case-fold-search t)
  (setq buffer-read-only t)
  (if (fboundp 'popup-mode-menu)
      (setq mode-popup-menu id-popup-br-menu))
  (run-hooks 'br-class-list-hook)
  (run-hooks 'br-mode-hook))

(defun br-next-buffer (&optional special alt-prefix)
  "Returns next sequential browser buffer or special one if optional SPECIAL is non-nil.
Non-nil ALT-PREFIX is used as prefix in buffer name."
  (let* ((suffix (concat special
			 (if (and (stringp special)
				  (string-match "[0-9]\\'" special))
			     nil
			   (int-to-string (1+ (or (br-class-level) 0))))))
	 (buf (get-buffer-create
	       (concat (or alt-prefix br-buffer-prefix)
		       (if (integerp suffix)
			   (int-to-string suffix)
			 suffix)))))
    (if buf (progn
	      (or special (br-next-listing-window))
	      (set-window-buffer (selected-window) buf)
	      (let (buffer-read-only)
		(kill-all-local-variables)
		(buffer-disable-undo (current-buffer))
		;; Clear out any feature tags that may have been associated
		;; with this buffer, so we don't mistakenly reference them.
		(br-feature-clear-tags)
		(erase-buffer))
	      (make-local-variable 'frame-title-format)
	      (setq frame-title-format
		    (list "OO-Browser-" br-version "@" (system-name) ":  "
			  br-env-name " - " br-env-file))
	      (br-mode)
	      (br-set-mode-line)
	      (set-buffer-modified-p nil)))
    buf))

(defun br-next-listing-window (&optional prev)
  "Move to next browser listing window (non-viewer window).
Optional PREV means to previous window."
  ;; The `br-non-listing-window-p' call below calls `br-in-browser' which if
  ;; called from `br-window-setup' when initializing browser windows can
  ;; set the variable, br-in-browser, to nil since it doesn't recognize the
  ;; window configuration.  Prevent this by making the variable local for this
  ;; function.
  (let ((owind (selected-window))
	(br-in-browser br-in-browser))
    (while (progn (other-window (if prev -1 1))
		  (if (br-non-listing-window-p)
		      (not (eq (selected-window) owind))))
      (setq br-in-browser (selected-frame)))))

(defun br-pathname (filename)
  "Return full pathname for FILENAME in browser Elisp directory."
  (if br-directory
      (expand-file-name filename br-directory)
    (error "The `br-directory' variable must be set to a string value.")))

(defun br-resize (min-width)
  "Resize browser listing windows to have MIN-WIDTH."
  (interactive)
  (let* ((window-min-width 3)
	 (oldn (1- (length (br-window-list))))
	 (n (max 1 (/ (frame-width) min-width)))
	 (numw n)
	 (diff (- numw oldn))
	 (width (/ (frame-width) numw))
	 (obuf (current-buffer)))
    (br-to-first-list-window)
    (cond ((= diff 0)
	   (br-resize-windows numw width))
	  ((> diff 0)
	   (setq n oldn)
	   (while (> n 1)
	     (setq n (1- n))
	     (shrink-window-horizontally (max 0 (- (window-width)
						   min-width)))
	     (br-next-listing-window))
	   (setq n diff)
	   (while (> n 0)
	     (setq n (1- n))
	     (split-window-horizontally (max window-min-width
					     (- (window-width)
						min-width)))) 
	   (setq n oldn)
	   (while (< n numw)
	     (setq n (1+ n))
	     (br-next-listing-window)
	     (br-next-buffer n br-buffer-prefix-blank))
	   (br-to-first-list-window)
	   (br-resize-windows numw width)
	   )
	  (t  ;; (< diff 0)
	   (while (> n 0)
	     (setq n (1- n))
	     (br-next-listing-window))
	   (setq n (- diff))
	   (while (> n 0)
	     (setq n (1- n))
	     (delete-window))
	   (br-to-first-list-window)
	   (br-resize-windows numw width)
	   ))
    (setq br-min-width-window min-width)
    (let ((owind (get-buffer-window obuf)))
      (if owind
	  (select-window owind)
	(br-to-view-window)
	(br-next-listing-window)))))

(defun br-resize-narrow ()
  "Narrow listing windows by 10 characters."
  (interactive)
  (if (<= window-min-width (- br-min-width-window 10))
      (br-resize (max window-min-width (- br-min-width-window 10)))
    (beep)))

(defun br-resize-widen ()
  "Widen listing windows by 10 characters."
  (interactive)
  (if (and (>= (frame-width) (+ br-min-width-window 10))
	   (> (length (br-window-list)) 2))
      (br-resize (min (frame-width) (+ br-min-width-window 10)))
    (beep)))

(defun br-resize-windows (n width)
  (while (> n 1)
    (setq n (1- n))
    (shrink-window-horizontally (- (window-width) width))
    (br-next-listing-window)))

(defun br-set-mode-line ()
  "Set mode line string."
  (setq mode-line-format (list "  %17b --" '(-3 . "%p") "-%-")
	mode-line-buffer-identification (list (buffer-name)))
  (set-buffer-modified-p t))

(defun br-setup-next-window (command-string)
  "Setup to display OO-Browser command output in the next listing window.
COMMAND-STRING is usually a one character mnemonic string for the command
generating the output for the window.  It is added to the buffer name
preceding a level number to aid the user in navigation."
  (let ((next-level
	 (if (and (stringp command-string)
		  (string-match "[0-9]\\'" command-string))
	     nil
	   (int-to-string (1+ (or (br-class-level) 0))))))
    (br-add-level-hist)
    (br-next-listing-window)
    (br-next-buffer (concat command-string next-level))))

(defun br-show-classes (func top-only-flag &optional uniq command-string)
  "Display a list of classes generated by calling FUNC.
TOP-ONLY-FLAG means only top-level classes (those that don't inherit from any
other non-abstract class) are listed.  Optional UNIQ means sort and eliminate
duplicates.
COMMAND-STRING is a short mnemonic string to attach to the listing buffer name
to help describe the listing command used."
  (message "Ordering classes...")
  (let ((classes (funcall func)))
    (setq classes (br-class-list-filter classes top-only-flag))
    (br-clear command-string)
    (let (buffer-read-only)
      (erase-buffer)
      (br-insert-classes classes)
      (if uniq
	  (progn
	    (if (stringp br-sort-options)
		(call-process-region (point-min) (point-max) "sort" t t nil
				     br-sort-options)
	      (call-process-region (point-min) (point-max) "sort" t t nil))
	    (if (or (and (stringp br-sort-options)
			 (string-match "u" br-sort-options))
		    ;; Then sort made the list of elements unique, so
		    ;; do nothing, or if can't find uniq program, do
		    ;; nothing.
		    (not (locate-file "uniq" exec-path ":.exe")))
		nil
	      (call-process-region (point-min) (point-max) "uniq" t t))))))
  (goto-char (point-min))
  (message "Ordering classes...Done"))

(defun br-this-level-classes (&optional keep-indent)
  "Return the class entries from the current browser listing.
Optional KEEP-INDENT non-nil means keep indentation preceding class name."
  (let ((classes)
	(feature-regexp (format "^[ \t]*%s " br-feature-type-regexp)))
    (save-excursion
      (goto-char (point-min))
      (while (and (not (looking-at "^[ \t]*$"))
		  (if (and
		       ;; Treat protocol/interface and category entries as classes.
		       (not (looking-at "^[ \t]*[<\(]"))
		       ;; Ignore feature entries
		       (looking-at feature-regexp))
		      t ;; skip this entry
		    ;; assume is a class
		    (setq classes (cons (br-find-class-name keep-indent)
					classes)))
		  (= (forward-line 1) 0))))
    (nreverse (delq nil classes))))

(defun br-this-level-entries ()
  "Return list of all entries in the current listing."
  (let ((entries)
	(feature-regexp (format "^[ \t]*%s " br-feature-type-regexp)))
    (save-excursion
      (goto-char (point-min))
      (while (and (not (looking-at "^[ \t]*$"))
		  (if (looking-at feature-regexp)
		      ;; a feature
		      (setq entries (cons (br-find-feature-entry) entries))
		    ;; assume is a class
		    (setq entries (cons (br-find-class-name) entries)))
		  (= (forward-line 1) 0))))
    (nreverse (delq nil entries))))

(defun br-this-level-features ()
  "Return list of features in the current listing."
  (let ((feature-regexp (concat "[ \t]*" br-feature-entry-regexp))
	(feature-list))
    (save-excursion
      (goto-char (point-min))
      (while (progn (if (looking-at feature-regexp)
			(setq feature-list
			      (cons (br-find-feature-entry) feature-list)))
		    (= (forward-line 1) 0))))
    (nreverse (delq nil feature-list))))

(defun br-to-first-list-window ()
  (br-to-view-window)
  (br-next-listing-window))

(defun br-to-tree ()
  "If point is within ellipses (...), move to the inheritance expansion for the current class."
  (if (save-excursion
	(skip-chars-backward ".")
	(looking-at "\\.\\.\\."))
      (progn (beginning-of-line)
	     (let ((class-expr (concat "^[ \t]*"
				       (br-find-class-name)
				       "$")))
	       (if (re-search-backward class-expr nil t)
		   (progn (skip-chars-forward " \t")
			  (recenter '(4))
			  t))))))

(defun br-to-view-window ()
  "Move to viewer window."
  (if (br-in-view-window-p)
      nil
    (setq *br-prev-listing-window* (selected-window))
    (while (and (not (br-in-view-window-p))
		(progn (other-window 1)
		       (not (eq (selected-window)
				*br-prev-listing-window*)))))))

(defun br-window-setup ()
  (and (fboundp 'modify-frame-parameters)
       (cdr (assq 'unsplittable (frame-parameters)))
       (modify-frame-parameters (selected-frame) '((unsplittable))))
  (delete-other-windows)
  ;; Set top of frame line in case it is not 0.
  (or (fboundp 'window-highest-p)
      (setq br-top-of-frame (nth 1 (window-edges))))
  (split-window-vertically nil)
  (let* ((n (max 1 (/ (frame-width) br-min-width-window)))
	 (width (/ (frame-width) n))
	 (start-win))
    ;; `A' means all classes will be listed, 1 = first buffer
    (br-next-buffer "A1")
    (while (> n 1)
      (setq n (1- n))
      (if (<= (window-width (selected-window)) width)
	  (progn (setq start-win (selected-window))
		 (while (and (progn (other-window 1)
				    (if (fboundp 'window-highest-p)
					(window-highest-p (selected-window))
				      (= (nth 1 (window-edges))
					 br-top-of-frame)))
			     (not (eq (selected-window) start-win))
			     (<= (window-width (selected-window)) width)))))
      (split-window-horizontally width)
      (br-next-buffer nil br-buffer-prefix-blank)))
  ;;
  ;; Leave point in the first window
  (br-to-view-window)
  (other-window 1))

(defun br-view-ext-start (viewer-cmd name file line-num)
  "Start an external viewer given by VIEWER-CMD using NAME applied to FILE at LINE-NUM."
  (apply 'start-process name name viewer-cmd
	 (if (equal viewer-cmd "xterm")
	     (nconc (list "-title" (if (stringp br-vw2)
				       (concat br-vw2 ": " file)
				     file))
		    (delq nil (list br-vw1 br-vw2 
				    (if line-num (format "+%s" line-num))
				    br-vw3 br-vw4 br-vw5 br-vw6 br-vw7 br-vw8
				    br-vw9))
		    (list file))
	   (nconc (if line-num (list (format "+%s" line-num)))
		  (delq nil (list br-vw1 br-vw2 br-vw3 br-vw4 br-vw5 br-vw6
				  br-vw7 br-vw8 br-vw9))
		  (list file)))))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar br-ancestor-function nil
  "If non-nil, a function of 3 arguments called after each ancestor class is inserted into an ancestry listing.
First argument is the class just inserted, second argument is a flag
indicating whether class has previously been displayed within the listing and
third argument is the number of spaces to indent each feature entry for this
class.")

(defvar br-top-of-frame 0
  "Frame-relative line number at which the OO-Browser frame's uppermost windows start.")

(defvar br-ed-num 0)
(defvar br-ed-name "extEd")
(defvar br-vw-num 0)
(defvar br-vw-name "extVw")

(defvar br-in-browser nil
  "Equal to the frame displaying the OO-Browser when in use, else nil.")

(defvar br-lib-search-dirs nil
  "List of directories below which OO source files and other library
directories are found.  A library is a stable group of OO classes.  Do not
set this variable directly.  Each OO language library which invokes
`br-browse' should set it.")

(defvar br-sys-search-dirs nil
  "List of directories below which OO source files and other system
directories are found.  A system is a group of OO classes that are likely to
change.  Do not set this variable directly.  Each OO language library which
invokes `br-browse' should set it.")

(defvar *br-level-hist* nil
  "Internal history of visited listing windows and buffers.")

(defvar *br-prev-listing-window* nil
  "Saves listing window used prior to viewer window entry.
Allows return to previous listing window when done with the viewer.")

(defvar *br-prev-wconfig* nil
  "Saves window configuration prior to browser entry.")

(defvar *br-save-wconfig* nil
  "Saves window configuration between invocations of the browser.")

(defconst br-buffer-prefix-inher "OO-Browse-")
(defconst br-buffer-prefix-blank "Blank-")
(defconst br-buffer-prefix-info "*OO-Browser ")
(defvar br-buffer-prefix br-buffer-prefix-inher
  "Browser buffer name prefix.")


(defvar br-mode-map nil
  "Keymap containing OO-Browser commands.")
(if br-mode-map
    nil
  (setq br-mode-map (make-keymap))
  (suppress-keymap br-mode-map)
  (define-key br-mode-map "@"        'br-at)
  (define-key br-mode-map "="        'br-attributes)
  (define-key br-mode-map "\<"       'br-viewer-beginning-of-buffer)
  (define-key br-mode-map "\>"       'br-viewer-end-of-buffer)
  (define-key br-mode-map ","        'br-viewer-scroll-down-by-line)
  (define-key br-mode-map "."        'br-viewer-scroll-up-by-line)
  (define-key br-mode-map "1"        'br-view-full-frame)
;  (define-key br-mode-map "\C-c^"    'br-add-class-file)
  (define-key br-mode-map "a"        'br-ancestors)
  (define-key br-mode-map "A"        'br-show-all-classes)
  (define-key br-mode-map "\M-a"     'br-name-add)
  (define-key br-mode-map "b"        'br-buffer-menu)
  (define-key br-mode-map "\C-c\C-b" 'br-report-bug)
  (define-key br-mode-map "c"        'br-children)
  (define-key br-mode-map "C"        'br-categories)
  (define-key br-mode-map "\M-c"     'br-class-stats)
  (define-key br-mode-map "\C-c\C-c" 'br-env-browse)
  (define-key br-mode-map "d"        'br-descendants)
  (define-key br-mode-map "\C-c\C-d" 'br-delete)
  ;; {M-d} is used down below for `br-tree'
  (define-key br-mode-map "e"        'br-edit-entry)
  (define-key br-mode-map "\M-e"     'br-env-stats)
  (define-key br-mode-map "\C-c\C-e" 'br-env-rebuild)
  (define-key br-mode-map "f"        'br-features)
  (define-key br-mode-map "F"        'br-feature-signature)
  ;; {M-f} is used down below for `br-tree-features-toggle'
  ;; {M-g} is used down below for `br-tree-graph'
  (define-key br-mode-map "?"        'br-help)
  (define-key br-mode-map "h"        'br-help)
  (define-key br-mode-map "H"        'br-help-ms) ;; mouse help
  (define-key br-mode-map "i"        'br-entry-info)
  (define-key br-mode-map "I"        'br-implementors)
  (define-key br-mode-map "j"        'br-feature-view-declaration)
  (define-key br-mode-map "J"        'br-feature-edit-declaration)
  (define-key br-mode-map "\C-c\C-k" 'br-kill)
  ;; {M-k} is used down below for `br-tree-kill'
  (define-key br-mode-map "l"        'br-lib-top-classes)
  (define-key br-mode-map "L"        'br-lib-rebuild)
  (define-key br-mode-map "\M-l"     'br-names-display)
  (define-key br-mode-map "\C-c\C-l" 'br-env-load)
  (define-key br-mode-map "m"        'br-match)
  (define-key br-mode-map "M"        'br-match-entries)
  (define-key br-mode-map "\M-m"     'br-name-remove)
  ;; "\C-c\C-m" is reserved for future use.
  (define-key br-mode-map "\M-n"     'br-name-change)
  (define-key br-mode-map "\C-n"     'br-next-entry)
  (define-key br-mode-map "o"        'br-order)
  (define-key br-mode-map "p"        'br-parents)
  (define-key br-mode-map "P"        'br-protocols)
  (define-key br-mode-map "\C-p"     'br-prev-entry)
  (define-key br-mode-map "q"        'br-quit)
  (define-key br-mode-map "r"        'br-routines)
  (define-key br-mode-map "\M-r"     'br-name-replace)
  (define-key br-mode-map "\C-c\C-r" 'br-refresh)
  (define-key br-mode-map "s"        'br-sys-top-classes)
  (define-key br-mode-map "S"        'br-sys-rebuild)
  (define-key br-mode-map "\C-c\C-s" 'br-env-save)
  (define-key br-mode-map "t"        'br-show-top-classes)
  (define-key br-mode-map "T"        'br-show-top-classes)
  (define-key br-mode-map "u"        'br-unique)
  (define-key br-mode-map "v"        'br-view-entry)
  (define-key br-mode-map "V"        'br-view-friend)
  (define-key br-mode-map "\C-c\C-v" 'br-to-from-viewer)
  (define-key br-mode-map "\C-c\C-w" 'br-write-buffer)
  (define-key br-mode-map "w"        'br-where)
  (define-key br-mode-map "x"        'br-exit-level)
  (define-key br-mode-map "\C-x-"    'br-resize-narrow)
  (define-key br-mode-map "\C-x+"    'br-resize-widen)
  (define-key br-mode-map "#"        'br-count)
  (define-key br-mode-map "\C-c#"    'br-version)
  (define-key br-mode-map " "        'br-viewer-scroll-up)
  (define-key br-mode-map "\177"     'br-viewer-scroll-down)
  (if (string-match "XEmacs" emacs-version)
      (define-key br-mode-map '[backspace] 'br-viewer-scroll-down))
  ;;
  ;; Mouse keys
  (cond ((fboundp 'popup-mode-menu) nil)
	(hyperb:xemacs-p
	 (define-key br-mode-map 'button3 'br-popup-menu)
	 (define-key br-mode-map 'button3up nil))
	(t ;; hyperb:emacs19-p
	 (define-key br-mode-map [down-mouse-3] 'br-popup-menu)
	 (define-key br-mode-map [mouse-3] nil)))
  ;;
  ;; Define graphical browser keys if a window system is available.
  (if hyperb:window-system
      (progn (require 'br-tree)
	     (define-key br-mode-map "\M-d" 'br-tree)
	     (define-key br-mode-map "\M-f" 'br-tree-features-toggle)
	     (define-key br-mode-map "\M-g" 'br-tree-graph)
	     (define-key br-mode-map "\M-k" 'br-tree-kill))))

(defvar br-tmp-class-set nil
  "Set of classes created for temporary use by br-*-trees functions.")
(defvar br-tmp-depth 0
  "Temporary variable indicating inheritance depth of class in `br-ancestor-trees-inverted'.")

(provide 'br)
