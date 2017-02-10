;;!emacs
;;
;; FILE:         br-name.el
;; SUMMARY:      Maintain a user-specific set of names associated with OO-Browser Environments.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     oop, tools 
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:    22-Aug-98 at 00:46:02
;; LAST-MOD:     15-Aug-99 at 05:18:03 by Bob Weiner
;;
;; Copyright (C) 1998  BeOpen.com
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hasht)
(require 'hversion)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar br-env-name nil
  "Unique user-specific name for the current OO-Browser Environment.
Valid values are:
  nil      - not yet initialized for the current Environment;
  t        - there is no user-specific name for this Environment; use its
             filename in operations instead
  <string> - Env name")

(defvar br-names-file
    (if hyperb:microcruft-os-p
	"c:/_oo-browser"
      (expand-file-name ".oo-browser" (concat "~" (user-real-login-name))))
  "File which stores OO-Browser unique Environment name to pathname associations.
Its value is ~/.oo-browser or c:/_oo-browser (under MS OSes).")

(defvar br-names-htable (hash-make 1)
  "Hash table of OO-Browser-Env-Name keys and Env-File values.
Its entries are user-specific; they are read from the ~/.oo-browser or
~/_oo-browser file (under MS OSes).")

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar br-names-alist t
  "Temporary alist of (OO-Browser-Env-File . Env-Name) read from `br-names-file'.
Value of `t' means it has not yet been initialized from the file.")

(defvar br-names-menu-cache t
  "Cache of menu items which load Environments by name.
Value of `t' means it needs to be reinitialized.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(if (fboundp 'abbreviate-file-name)
    (if (string-match "XEmacs" emacs-version)
	(defun br-abbreviate-file-name (filename)
	  "Shorten FILENAME as much as possible based on `directory-abbrev-alist' and home directory."
	  (abbreviate-file-name filename t))
      (defalias 'br-abbreviate-file-name 'abbreviate-file-name))
  (defun br-abbreviate-file-name (filename)
    "Return filename unchanged since no abbreviation function is available."
    filename))

(defun br-name-add (env-name env-file)
  "Add a unique ENV-NAME string associated with ENV-FILE.
Return the unique ENV-NAME.  If ENV-NAME is nil, it is interactively read."
  (interactive
   (list (setq env-name (br-name-read "Unique name for Environment: " nil))
	 (br-env-read-file-name
	  (format "Associate `%s' with file: " env-name))))
  (if (and (null env-name) (stringp env-file))
      (setq env-name (br-name-read
		      (format "Give name to \"%s\": "
			      (br-abbreviate-file-name env-file))
		      nil)))
  (if (eq env-name t)
      t
    (br-name-validate-arg-string "env-name" env-name)
    (br-name-validate-arg-string "env-file" env-file)
    (setq env-file (expand-file-name env-file))
    (if (br-name-get-env-file env-name)
	(error "(OO-Browser): Env `%s' exists, try renaming or removing it."
	       env-name)
      (hash-add env-file env-name br-names-htable)
      (br-names-save)
      env-name)))

(defun br-name-change (env-name new-env-name)
  "Change ENV-NAME to NEW-ENV-NAME."
  (interactive
   (progn (setq env-name (br-name-read "Change Environment name: " t
				       br-env-name))
	  (list env-name
		(br-name-read
		 (format "Change Environment name `%s' to: " env-name)
		 nil))))
  (br-name-validate-arg-string "env-name" env-name)
  (br-name-validate-arg-string "new-env-name" new-env-name)
  (let ((env-file (br-name-get-env-file env-name)))
    (if env-file
	(progn
	  (hash-delete env-name br-names-htable)
	  (hash-add env-file new-env-name br-names-htable)
	  (br-names-save)
	  (if (interactive-p)
	      (message "Environment name `%s' changed to `%s'"
		       env-name new-env-name)))
      (error "(OO-Browser): br-name-change - `%s' does not exist"
	     env-name))))

(defun br-name-get-env-file (env-name)
  "Return Env file associated with ENV-NAME."
  (if (stringp env-name)
      (hash-get env-name br-names-htable)))

(defun br-name-get (env-file)
  "Return the first Env name associated with ENV-FILE or nil."
  (br-name-validate-arg-string "env-file" env-file)
  (setq env-file (expand-file-name env-file))
  (catch 'env-name
    (hash-map
     (function (lambda (pathname-name-cons)
		 (if (string-equal env-file (car pathname-name-cons))
		     (throw 'env-name (cdr pathname-name-cons)))))
     br-names-htable)
    nil))

(defun br-name-read (&optional prompt must-match initial-name)
  "Interactively PROMPT for and return an existing (if MUST-MATCH is non-nil) OO-Browser Environment name, starting with INITIAL-NAME.
All arguments are optional.  Return nil if no such names exist."
  (if (or (and must-match (hash-empty-p br-names-htable))
	  noninteractive)
      t
    (let ((env-name)
	  (completion-ignore-case t))
      (while (null env-name)
	(setq env-name
	      (completing-read (or prompt "Environment name: ")
			       (hash-obarray br-names-htable)
			       nil must-match initial-name)
	      ;; Clear out initial-name so it does not reappear each time
	      ;; the user is re-prompted for a name.
	      initial-name nil)
	(cond ((and (stringp env-name) (string-equal env-name ""))
	       ;; Return t to signal no Env name selection.
	       (setq env-name t))
	      ((and (stringp env-name) (string-match "\\`\\s-*\\'" env-name))
	       (beep) (setq env-name nil))))
      env-name)))

(defun br-name-remove (env-name)
  "Remove ENV-NAME's association with an Env file.
This does not delete the associated Env file.
Return non-nil iff ENV-NAME is associated with an Env file."
  (interactive (list (br-name-read "Remove Environment named: " t
				   br-env-name)))
  (br-name-validate-arg-string "env-name" env-name)
  (prog1 (hash-delete env-name br-names-htable)
    (br-names-save)
    (if (interactive-p)
	(message "Environment name `%s' removed" env-name))))

(defun br-name-replace (env-name new-env-file)
  "Replace ENV-NAME's associated Env file with NEW-ENV-FILE."
  (br-name-validate-arg-string "env-name" env-name)
  (br-name-validate-arg-string "new-env-file" new-env-file)
  (setq new-env-file (expand-file-name new-env-file))
  (hash-add new-env-file env-name br-names-htable)
  (br-names-save)
  (if (interactive-p)
      (message "`%s' associated with \"%s\"" env-name new-env-file)))

(defun br-names-display ()
  "Display the user-specific list of OO-Browser Environment names and files."
  (interactive)
  (if (fboundp 'with-displaying-help-buffer)
      (with-displaying-help-buffer 'br-names-display-internal
				   "*OO-Browser Environments*")
    (with-output-to-temp-buffer "*OO-Browser Environments*"
      (br-names-display-internal))))

(defun br-names-empty-p ()
  "Return t if there no Environment names have been added or loaded, else nil."
  (or (not (hashp br-names-htable)) (hash-empty-p br-names-htable)))

(defun br-names-initialize ()
  "Initialize Env name to file associations if not already done."
  (if (eq br-names-alist t)
      (br-names-read-file nil)))

(defun br-names-list ()
  "Return the user-specific list of OO-Browser (Environment-File . Environment-name) pairs."
  (hash-map 'identity br-names-htable))

;;;###autoload
(defun br-names-menu (menu-items)
  "Return an unnamed menu of commands that load a user's named OO-Browser Environments."
  (if (eq br-names-menu-cache t)
      (progn
	(br-names-initialize)
	(if (hash-empty-p br-names-htable)
	    (setq br-names-menu-cache nil)
	  (setq br-names-menu-cache
		(append
		 (sort
		  (hash-map
		   (if (featurep 'infodock)
		       (function
			(lambda (pathname-name-cons)
			  (vector (cdr pathname-name-cons)
				  `(let ((id-tool-visible-flag 'visible))
				     (id-tool '(br-env-browse
						,(car pathname-name-cons)
						,(cdr pathname-name-cons))
					      'OO-Browser 'br-mode 1))
				  t)))
		     (function (lambda (pathname-name-cons)
				 (vector (cdr pathname-name-cons)
					 `(br-env-browse
					   ,(car pathname-name-cons)
					   ,(cdr pathname-name-cons))
					 t))))
		   br-names-htable)
		  (function (lambda (menu-item1 menu-item2)
			      (string-lessp (elt menu-item1 0)
					    (elt menu-item2 0)))))
		 menu-items))))
    br-names-menu-cache))

(defun br-names-read-file (&optional env-names-file)
  "Read from optional ENV-NAMES-FILE or `br-names-file' Env name and file associations.
If the file to be read does not exist, nothing is done and nil is returned.
If the file is unreadable or not of the right format, an error is signaled.
If the file is read properly, `br-names-htable' is initialized and t is
returned."
  (interactive (list
		(read-file-name "Read Environment names file: "
				nil
				br-names-file t)))
  (or env-names-file (setq env-names-file br-names-file))
  (br-name-validate-arg-string "env-names-file" env-names-file)
  (if (not (file-exists-p env-names-file))
      ;; Do nothing when file does not exist.
      nil
    (if (not (file-readable-p env-names-file))
	(error "(OO-Browser): \"%s\" does not exist or is unreadable"
	       env-names-file))
    ;; Should set `br-names-alist'.
    (condition-case ()
	(progn (load-file env-names-file)
	       (if (eq br-names-alist t)
		   (signal-error 'error nil)
		 (mapcar
		  (function (lambda (pathname-name-cons)
			      (hash-add (car pathname-name-cons)
					(cdr pathname-name-cons)
					br-names-htable)))
		  br-names-alist)
		 t))
      (error (error "(OO-Browser): \"%s\" is an invalid Environment names file"
		    env-names-file)))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun br-names-display-internal ()
  (if (featurep 'hyperbole)
      (princ "Press the Action Key within a pathname to load that Environment:\n\n"))
  (mapcar
   'princ
   (sort (hash-map 
	  (function
	   (lambda (pathname-name-cons)
	     (format (format "%%-%ds - \"%%s\"\n"
			     (apply 'max
				    (hash-map
				     (function
				      (lambda (pathname-name-cons)
					(length (cdr pathname-name-cons))))
				     br-names-htable)))
		     (cdr pathname-name-cons)
		     (br-abbreviate-file-name (car pathname-name-cons)))))
	  br-names-htable) 'string-lessp)))

(defun br-name-validate-arg-string (arg-name arg-val)
 (cond ((or (eq arg-val t) (equal arg-val ""))
	(error "(OO-Browser): Aborting command, no value given for `%s'"
	arg-name))
       ((not (stringp arg-val))
	(error "(OO-Browser): Invalid `%s' value, `%s'"
	       arg-name arg-val))
       (t)))

(defun br-names-save ()
  "Save the user-specific list of existing OO-Browser Environment names."
  ;; Force reinitialization of names menu.
  (setq br-names-menu-cache t)
  (save-excursion
    (let ((standard-output (set-buffer (find-file-noselect br-names-file))))
      (setq buffer-read-only nil)
      (erase-buffer)
      (princ "(setq br-names-alist\n'")
      (if (hash-empty-p br-names-htable)
	  (princ nil)
	(hash-prin1 br-names-htable))
      (princ ")\n")
      (save-buffer))))

(provide 'br-name)
