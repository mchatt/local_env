;;!emacs
;;
;; FILE:         br-env.el
;; SUMMARY:      OO-Browser Environment support functions.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:     8-Jun-90
;; LAST-MOD:     18-Apr-01 at 16:26:50 by Bob Weiner
;;
;; Copyright (C) 1989-1995, 1997, 1998 BeOpen.com
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'br-start)
(require 'hasht)
(require 'hypb)
(require 'br-name)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(if (fboundp 'file-relative-name)
    nil
  ;; For V18 Emacs
  (defun file-relative-name (filename &optional directory)
    "Convert FILENAME to be relative to DIRECTORY (default: default-directory)."
    (setq filename (expand-file-name filename)
	  directory (file-name-as-directory (if directory
						(expand-file-name directory)
					      default-directory)))
    (while directory
      (let ((up (file-name-directory (directory-file-name directory))))
	(cond ((and (string-equal directory up)
		    (file-name-absolute-p directory))
	       ;; "/"
	       (setq directory nil))
	      ((string-match (concat "\\`" (regexp-quote directory))
			     filename)
	       (setq filename (substring filename (match-end 0)))
	       (setq directory nil))
	      (t
	       ;; go up one level
	       (setq directory up)))))
    filename))

;; NOTE: First argument to this function must remain `env-file'
;; because of the way it is called from "hpath.el".
;;;###autoload
(defun br-env-browse (env-file &optional env-name)
  "Invoke the OO-Browser on an existing or to be created Environment ENV-FILE or ENV-NAME."
  (interactive
   (progn (br-names-initialize)
	  (setq env-name
		(br-name-read "Load/Create OO-Browser Env named: " nil))
	  (setq env-file (or (br-name-get-env-file env-name)
			     (br-env-read-file-name
			      (if (or (eq env-name t) (equal env-name ""))
				  "Load/Create unnamed OO-Browser Env file: "
				(format "Associate `%s' with file: " env-name))
			      default-directory
			      (expand-file-name br-env-default-file))))
	  (if (eq env-name t)
	      ;; br-env-name must be set here to prevent the name of the
	      ;; previously loaded Env from remaining after the new unnamed
	      ;; Env is loaded.
	      (setq br-env-name t))
	  (list env-file env-name)))

  (let ((file-name-cons (br-env-validate-arg-strings
			 "br-env-browse" env-file env-name)))
    (setq env-file (car file-name-cons)
	  env-name (cdr file-name-cons)))
  ;; Save env-name permanently so is not lost if an error is signalled
  ;; anywhere below.
  (or (br-name-get-env-file env-name)
      (br-name-add env-name env-file))
  (cond ((and (file-exists-p env-file)
	      (not (file-readable-p env-file)))
	 (error "(br-env-browse): Env file `%s' is unreadable." env-file))
	((not (file-exists-p env-file))
	 ;; Specify a new Environment
	 (funcall (intern-soft (concat (br-env-select-lang) "browse"))
		  env-file))
	(t;; Existing Environment
	 (let ((lang-string (br-env-read-language-prefix env-file)))
	   (if lang-string
	       (funcall (intern-soft (concat lang-string "browse"))
			env-file)
	     (error "(br-env-browse): Invalid env file: `%s'" env-file))))))

(defun br-env-build (&optional env-file env-name background-flag no-load)
  "Build and load Environment from spec given by optional ENV-FILE, ENV-NAME or `br-env-file'.

If optional 2nd argument BACKGROUND-FLAG is t, build the Environment using a
background process.  If it is nil, build in foreground.  If it is the symbol,
debug, build in the background and show a debug backtrace if any error occurs
\(under InfoDock and XEmacs only).  Any other value prompts for whether to
build in the background if the `make' program is found within `exec-path'.

If optional 3rd argument NO-LOAD is non-nil, the Environment is not loaded by
this function (since `br-env-load' might call this itself)."
  (interactive
   (progn (br-names-initialize)
	  (setq env-name
		(br-name-read "Build OO-Browser Env named: " nil))
	  (setq env-file (or (br-name-get-env-file env-name)
			     (br-env-read-file-name
			      (if (or (eq env-name t) (equal env-name ""))
				  "Build unnamed OO-Browser Env file: "
				(format "Associate `%s' with file: " env-name))
			      default-directory
			      (expand-file-name br-env-default-file)
			      t)))
	  (list env-file env-name 'prompt nil)))
  (cond ((or (null background-flag) (eq background-flag t)
	     (eq background-flag 'debug))
	 (if (not (locate-file "make" exec-path ":.exe"))
	     (setq background-flag nil)))
	(noninteractive
	 (setq background-flag nil))
	(t (setq background-flag
		 (if (locate-file "make" exec-path ":.exe")
		     (y-or-n-p "Build Environment in the background? ")))))

  (let ((file-name-cons (br-env-validate-arg-strings
			 "br-env-build" env-file env-name)))
    (setq env-file (car file-name-cons)
	  env-name (cdr file-name-cons)))
  (if (or (not (stringp env-file)) (equal env-file ""))
      (setq env-file br-env-file))
  (setq env-file (expand-file-name env-file))
  (or (not (file-exists-p env-file)) (file-readable-p env-file)
      (error "Non-readable Environment file, \"%s\"" env-file))
  (or (file-writable-p env-file)
      (error "Non-writable Environment file, \"%s\"" env-file))

  (if background-flag
      (progn (setenv "OO_BROWSER_ENV" env-file)
	     (setenv "OO_BROWSER_ENV_NAME"
		     (cond ((eq env-name t) "t")
			   ((null env-name) "nil")
			   (t env-name)))
	     (setenv "EMACSLOADPATH" (mapconcat 'identity load-path
						(if hyperb:microcruft-os-p
						    ";" ":")))
	     (let ((default-directory br-directory))
	       (compile
		(format "make -f Make-Env%s %s"
			(if (and (boundp 'invocation-directory)
				 (boundp 'invocation-name)
				 (stringp invocation-directory)
				 (stringp invocation-name)
				 (file-directory-p invocation-directory)
				 (file-name-absolute-p invocation-directory))
			    (concat " EMACS="
				    (expand-file-name
				     invocation-name invocation-directory))
			  "")
			(if (eq background-flag 'debug)
			    "oo-browser-env-debug" "oo-browser-env")))))
    (or no-load (br-env-load env-file env-name nil t))
    (setq br-env-start-build-time (current-time-string))
    ;; Detach unneeded data so can be garbage collected.
    (br-env-create-alists)
    (br-env-create-htables)
    (if (and (boundp 'br-feature-tags-file) (stringp br-feature-tags-file))
	(progn
	  (if (not (file-writable-p br-feature-tags-file))
	      (error
	       "(br-env-build): %s is not writable" br-feature-tags-file))
	  (set-buffer (find-file-noselect br-feature-tags-file))
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (set-buffer-modified-p nil)))
    (br-build-sys-htable)
    (br-build-lib-htable)
    (br-feature-build-htables)
    (setq br-env-spec nil
	  br-env-end-build-time (current-time-string))
    (br-env-save)
    ;; Detach unneeded data so can be garbage collected.
    (br-env-create-alists)
    (if (or noninteractive no-load)
	nil
      (br-env-load env-file env-name nil t))))

(defun br-env-rebuild (debug-flag)
  "Rescan System and Library sources associated with the current Environment.
When given a prefix arg, DEBUG-FLAG, it will output a debugging backtrace if
any error occurs during scanning (InfoDock and XEmacs only)."
  (interactive "P")
  (cond ((null br-env-file)
	 (error "(br-env-rebuild): Load an Environment before calling this."))
	((interactive-p)
	 (if debug-flag
	     (if (y-or-n-p (format "Debug build of `%s'? "
				   (or (and (stringp br-env-name) br-env-name)
				       br-env-file)))
		 (br-env-build br-env-file br-env-name 'debug))
	   (if (y-or-n-p (format "Rebuild `%s'? "
				 (or (and (stringp br-env-name) br-env-name)
				     br-env-file)))
	       (br-env-build br-env-file br-env-name 'prompt))))
	(t (error "(br-env-rebuild): This must be called interactively."))))

(defun br-env-create (&optional env-file lang-prefix)
  "Create and save the specification of a new OO-Browser Environment.
Interactively prompt for the Environment file name or use optional ENV-FILE.
Interactively prompt for the Environment language to use or use optional
LANG-PREFIX as the language indicator.

If called interactively and presently in the OO-Browser and the current
Environment is the one that has been re-specified, automatically rebuild it.
Otherwise, prompt for whether to build the Environment.

Return the name of the Environment specification file that was created."
  (if (stringp env-file)
      (read-string
       "Hit RET to specify the code directories for the Environment. ")
    (setq env-file (or (br-name-get-env-file br-env-name)
		       (br-env-read-file-name
			(if (or (eq br-env-name t) (equal br-env-name ""))
			    "Create unnamed OO-Browser Env file: "
			  (format "Associate `%s' with file: " br-env-name))
			default-directory
			(expand-file-name br-env-default-file)))))
  ;; Between the time when a new Envir name was specified by the user and the 
  ;; call to this function, a cached set of Envir variables may have been
  ;; reloaded and this may have reset `br-env-name' to t even though a name
  ;; was given.  Ensure that any name available is reset here.
  (if (eq br-env-name t)
      (setq br-env-name (or (br-name-get env-file) t)))
  (let ((file-name-cons (br-env-validate-arg-strings
			 "br-env-create" env-file br-env-name)))
    (setq env-file (car file-name-cons)))

  ;; Display Env spec if a previous one existed.
  (and (equal env-file br-env-file) (file-readable-p env-file) (br-env-stats))
  (let ((prompt "Top-level system-specific code dir #%d (RET to end): ")
	(br-env-spec t)
	br-sys-search-dirs br-lib-search-dirs
	br-lang-prefix
	br-children-htable
	br-sys-paths-htable
	br-sys-parents-htable
	br-lib-paths-htable
	br-lib-parents-htable
	br-paths-htable
	br-parents-htable)
    (br-env-create-htables)
    (setq br-lang-prefix (or lang-prefix (br-env-select-lang))
	  br-sys-search-dirs (br-env-get-dirs prompt)
	  ;; The leading whitespace is to give a visual indication that 
	  ;; the time of directory being prompted for has changed from System 
	  ;; to Library.
	  prompt "  Top-level reusable code library dir #%d (RET to end): "
	  br-lib-search-dirs (br-env-get-dirs prompt))
    ;; Now since user has not aborted, set real variables
    (setq br-env-spec t)
    (br-env-save env-file)
    ;; If called interactively and re-specifying current Env, then also
    ;; rebuild it.
    (if (interactive-p)
	(if (equal env-file br-env-file)
	    (if (br-in-browser)
		;; auto-build
		(br-env-build
		 br-env-file br-env-name
		 (y-or-n-p
		  "The Environment will now be built; build it in the background? "))
	      (call-interactively 'br-env-build))))
    env-file))

;;;###autoload
(defun br-env-load (&optional env-file env-name prompt no-build)
  "Load an OO-Browser Environment or specification from optional ENV-FILE, ENV-NAME or `br-env-file'.
Non-nil PROMPT means prompt user before building the Environment.
Non-nil NO-BUILD means skip the build of the Environment entirely.
Return t if the load is successful, else nil."
  (interactive
   (progn (br-names-initialize)
	  (setq env-name
		(br-name-read "Load OO-Browser Env named: " t))
	  (setq env-file (or (br-name-get-env-file env-name)
			     (br-env-read-file-name
			      (if (or (eq env-name t) (equal env-name ""))
				  "Load Environment from file: "
				(format "Load `%s' from file: " env-name))
			      default-directory
			      (expand-file-name br-env-default-file)
			      t)))
	  (list env-file env-name nil nil)))
  (let ((file-name-cons (br-env-validate-arg-strings
			 "br-env-load" env-file env-name)))
    (setq env-file (car file-name-cons)
	  env-name (cdr file-name-cons)))
  (setq env-file (or (and (not (equal env-file "")) env-file)
		     (br-env-default-file))
	env-file (expand-file-name env-file))
  (let ((buf (get-file-buffer env-file)))
    (and buf (kill-buffer buf)))
  (let ((br-loaded))
    (if (file-readable-p env-file)
	(unwind-protect
	    (progn
	      (message "Loading Environment...")
	      (sit-for 1)
	      ;; Ensure spec, version, time and feature values are nil for
	      ;; old Environment files that do not contain a setting for
	      ;; these variables.
	      (setq br-env-spec nil br-env-version nil
		    br-env-start-build-time nil
		    br-env-end-build-time nil
		    br-features-alist nil
		    br-feature-paths-alist nil)

	      ;; Ensure that OO-Browser support libraries for the current
	      ;; language are loaded, since this function may be called
	      ;; without invoking the OO-Browser user interface.
	      ;; This must be called before the Env is loaded
	      ;; and before br-env-file is set or it may
	      ;; overwrite Env variable settings improperly.
	      (setq br-lang-prefix
		   (br-env-read-language-prefix env-file))
	      (let ((lang-symbol
		     (intern-soft (concat br-lang-prefix "browse")))
		    lang-function)
		(if lang-symbol
		    (progn (setq lang-function (symbol-function lang-symbol))
			   (if (and (listp lang-function)
				    (eq (car lang-function) 'autoload))
			       (load (car (cdr lang-function))))
			   ;; Initialize language-specific browser variables.
			   (funcall (intern-soft
				     (concat br-lang-prefix "browse-setup"))
				    env-file))))

	      (load-file env-file)
	      (setq br-env-file env-file
		    br-env-name env-name)
	      (br-init env-file) ;; initializes auxiliary Env file variables

	      ;; Prevent rebuilding of Environment
	      (setq br-lib-prev-search-dirs br-lib-search-dirs
		    br-sys-prev-search-dirs br-sys-search-dirs)

	      (cond
	       ((and br-env-spec (not no-build))
		(setq br-loaded
		      (br-env-cond-build
		       env-file env-name
		       (if prompt "Build Environment `%s' now? "))))
	       ;; Feature storage formats changed in V4.00, so all prior
	       ;; Environments are obsolete.
	       ((and (not no-build)
		     (or (null br-env-version)
			 (and (stringp br-env-version)
			      (string-lessp br-env-version "04.00"))))
		(setq br-loaded
		      (br-env-cond-build
		       env-file env-name
		       (if prompt
			   "Env `%s' format is obsolete, rebuild it now? ")))
		(if (not br-loaded)
		    (error "(OO-Browser): The Environment must be rebuilt before use.")))))
	  ;;
	  ;; Initialize OO-Browser Environment data structures in cases where
	  ;; the Environment was not just built.
	  (if (or br-env-spec br-loaded)
	      nil
	    (setq br-children-htable (hash-make br-children-alist)
		  br-features-htable (hash-make br-features-alist)
		  br-feature-paths-htable (hash-make br-feature-paths-alist)
		  br-sys-paths-htable (hash-make br-sys-paths-alist)
		  br-lib-paths-htable (hash-make br-lib-paths-alist)
		  br-sys-parents-htable
		  (hash-make br-sys-parents-alist)
		  br-lib-parents-htable
		  (hash-make br-lib-parents-alist)
		  )
	    (br-env-set-htables t)
	    (setq br-loaded t))
	  (if (and (fboundp 'br-in-browser) (br-in-browser))
	      (br-refresh))
	  (message "Loading Environment...Done"))
      (if (file-exists-p env-file)
	  (progn (beep)
		 (message "No read rights for Environment file, \"%s\"" env-file)
		 (sit-for 4))
	(setq br-loaded (br-env-load
			 (br-env-create env-file br-lang-prefix)
			 env-name t no-build))))
    br-loaded))

(defun br-env-save (&optional save-file)
  "Save the modified Environment to a file given by optional SAVE-FILE or `br-env-file'."
  (interactive (list (br-env-read-file-name "Save Environment to: ")))
  (if (or (not (stringp save-file)) (equal save-file ""))
      (setq save-file br-env-file))
  (setq save-file (expand-file-name save-file))
  (or (file-writable-p save-file)
      (error "Non-writable Environment file, \"%s\"" save-file))
  (let ((buf (get-file-buffer save-file)))
    (and buf (kill-buffer buf)))
  (let ((dir (or (file-name-directory save-file)
		 default-directory)))
    (or (file-writable-p dir)
	(error "Non-writable Environment directory, \"%s\"" dir)))
  (save-window-excursion
    (let ((standard-output (br-env-edit save-file))
	  br-sym)
      (erase-buffer)
      (princ "\n(setq\nbr-env-version")
      (print br-version) ;; Yes, we need to use `br-version' here.
      ;; Save last build times, nil if none.
      (princ "\nbr-env-start-build-time")
      (print br-env-start-build-time)
      (princ "\nbr-env-end-build-time")
      (print br-env-end-build-time)
      ;; Save search dir settings.
      (br-env-save-mult-vars (cons (car br-env-mult-vars) nil))
      ;; Save language prefix, flag of whether is a specification,
      ;; children table, features table and feature paths table.
      (mapcar (function
		(lambda (nm)
		  (if (setq br-sym (intern-soft (concat "br-" nm)))
		      (let ((nm-mid (string-match "-htable$" nm)))
			(if nm-mid
			    (progn (princ "\nbr-") (princ (substring nm 0 nm-mid))
				   (princ "-alist\n'")
				   (hash-prin1 (symbol-value br-sym)))
			  (terpri) (princ br-sym) (princ "\n'")
			  (prin1 (symbol-value br-sym)) (terpri))))))
	      br-env-single-vars)
      ;; Save paths and parents tables.
      (br-env-save-mult-vars (cdr br-env-mult-vars))
      (princ ")\n")
      (save-buffer)
      (kill-buffer standard-output))))

(defun br-env-stats (&optional arg)
  "Display a summary for the current Environment in the viewer window.
With optional prefix ARG, display class totals in the minibuffer."
  (interactive "P")
  (let ((env-file (br-abbreviate-file-name br-env-file)))
    (if arg
	(message (br-env-totals-minibuffer))
      (br-funcall-in-view-window
       (concat br-buffer-prefix-info "Info*")
       (function
	(lambda ()
	  (insert (cdr (assoc br-lang-prefix br-env-lang-name-alist)))
	  (if (stringp br-env-name)
	      (insert (format " Environment: `%s' - \"%s\""
			      br-env-name env-file))
	    (insert (format " Environment: \"%s\"" env-file)))
	  (center-line)
	  (insert "\n\n")
	  (insert (format "%s by version %s of the OO-Browser.\n\n"
			  (if br-env-spec "Specified but not yet built" "Built")
			  (or br-env-version "earlier than 02.09.03")))
	  (if br-env-spec
	      nil
	    (if br-env-start-build-time
		(insert (format "Start time of last build: %s\n"
				br-env-start-build-time)))
	    (if br-env-end-build-time
		(insert (format "  End time of last build: %s\n\n"
				br-env-end-build-time))))
	  (insert (br-env-totals) "\n\n")
	  (let ((undefined (br-undefined-classes)))
	    (if undefined
		(insert (format "Undefined classes: %s\n\n" undefined))))
	  (mapcar
	   (function
	    (lambda (sys-lib)
	      (insert (format "Top-level %s code directories:\n"
			      (car sys-lib)))
	      (if (cdr sys-lib)
		  (progn (mapcar
			  (function
			   (lambda (dir)
			     (or (equal dir "")
				 (insert
				  (format "\t%s\n"
					  (br-abbreviate-file-name dir))))))
				 (cdr sys-lib))
			 (insert "\n"))
		(insert "\t<None>\n\n"))))
	   (list (cons "system-specific" br-sys-search-dirs)
		 (cons "reusable library"  br-lib-search-dirs)))
	  (hypb:configuration)
	  (insert "\n\n")
	  (set-buffer-modified-p nil)))))))

(defun br-env-substitute-home (path)
  "If path is relative to user's home directory, shorten the path with ~/.
Return modified or unmodified path."
  (if path
      (let ((home (regexp-quote (expand-file-name "~"))))
	(if (equal (string-match home path) 0)
	    (concat "~" (substring path (match-end 0)))
	  path))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun br-env-add-ref-classes (&optional htable-type)
  "Add classes to Environment which are referenced in it but not defined.
With optional HTABLE-TYPE, affect only that part of the Environment.
HTABLE-TYPE may be \"sys\"or \"lib\".  By default, add to both Library and
whole Environment tables."
  ;;
  ;; This function must NOT call any `get-htable' type functions or it can
  ;; cause an infinite loop.
  (if (null htable-type) (setq htable-type "lib"))
  (let ((paths-htable (symbol-value
		       (intern-soft (concat "br-" htable-type
					    (if htable-type "-")
					    "paths-htable"))))
	(parents-htable
	 (symbol-value
	  (intern-soft (concat "br-" htable-type
			       (if htable-type "-")
			       "parents-htable"))))
	(parents-htable-name (concat htable-type "-parents"))
	(paths-htable-name (concat htable-type "-paths"))
	(parents (if (equal br-lang-prefix "java-") '("Object") nil))
	(class)
	(classes)
	(pars))
    (if paths-htable (setq classes (br-all-classes paths-htable)))
    (if parents-htable (setq pars (br-env-all-parents parents-htable)))
    (while pars
      (setq class (car pars)
	    pars (cdr pars))
      (if (or (null class) (br-member class classes))
	  nil
	(setq classes (cons class classes))
	(br-env-add-to-htables class parents parents-htable-name)
	(br-add-to-paths-htable
	 class br-null-path
	 (br-get-htable paths-htable-name))))
    (if (equal br-lang-prefix "java-")
	(br-env-add-to-htables "Object" nil parents-htable-name))))

(defun br-env-add-to-htables (class parents parents-htable-name)
  "Add CLASS with a list of PARENTS to PARENTS-HTABLE-NAME.
PARENTS-HTABLE-NAME may be \"parents\", \"sys-parents\", or \"lib-parents\"."
  (if (null class)
      nil
    (setq parents-htable-name
	  (symbol-value (intern-soft (concat "br-" parents-htable-name "-htable"))))
    (if parents-htable-name (hash-add parents class parents-htable-name))))

(defun br-env-all-parents (&optional htable-type)
  "Return list of all parent names in Environment or optional HTABLE-TYPE.
HTABLE-TYPE may be \"sys\" or \"lib\". or an actual hash table."
  (apply 'append
	 (hash-map 'car
		   (cond ((and (stringp htable-type)
			       (not (string-equal htable-type "")))
			  (br-get-htable (concat htable-type "-parents")))
			 ((hashp htable-type) htable-type)
			 (t (br-get-parents-htable))))))

(defun br-env-batch-build ()
  "Build Environments from specifications while running Emacs in batch mode (background).
Invoke via a shell command line of the following form:
  cd <Br-Dir>; <Emacs> -batch -l ./br-start <Env-Spec-File-1> ... <Env-Spec-File-N> -f br-env-batch-build

where <Br-Dir>        = the directory in which your OO-Browser executable
                        Lisp code exists; 
      <Emacs>         = the executable name that you use to run emacs or InfoDock;
      <Env-Spec-File> = a full pathname to an OO-Browser created Environment
                        file, usually named \"OOBR\"."
  (br-init-autoloads)
  (if (or (not (boundp 'br-directory)) (null br-directory)
	  (not (file-exists-p br-directory)))
      (error "(br-env-batch-build): Set `br-directory' properly before use.")
    (let ((spec-file)
	  (files (delq nil (mapcar 'buffer-file-name (buffer-list)))))
      (while (setq spec-file (car files))
	(setq files (cdr files))
	(br-env-load spec-file nil nil t) ;; sets br-env-file
	(kill-buffer nil)
	(br-env-build spec-file nil nil nil)))))

;;; The following function is called by the compilation sentinel whenever a
;;; compilation finishes under versions of Emacs 19 or later.  (If you use
;;; Emacs 18, you would have to edit compilation-sentinel to call the
;;; function stored in `compilation-finish-function' as Emacs 19, compile.el
;;; does.
;;;
;;; If there already is a compilation-finish-function, save it and use it
;;; when not in a batch environment build.
(setq compilation-original-finish-function
      (and (boundp 'compilation-finish-function)
	   (not (eq compilation-finish-function 'br-env-batch-build-browse))
	   compilation-finish-function)
      compilation-finish-function 'br-env-batch-build-browse)

(defun br-env-batch-build-browse (&rest args)
  (cond ((and (boundp 'compilation-last-buffer)
	      (bufferp compilation-last-buffer))
	 (set-buffer compilation-last-buffer))
	((get-buffer "*compilation*")
	 (set-buffer "*compilation*")))
  (cond ((not (string-match "oo-browser-env" compile-command))
	 ;; Some other type of build.
	 (if compilation-original-finish-function
	     (apply compilation-original-finish-function args)))
	((not (and (stringp mode-line-process)
		   (string-match "exit \\(OK\\|\\[0\\]\\)" mode-line-process)))
	 ;; Build failed.
	 nil)
	(t ;; Environment build was successful.
	 (beep)
	 (let* ((env-file (getenv "OO_BROWSER_ENV"))
		(env-name (getenv "OO_BROWSER_ENV_NAME"))
		(prompt
		 (progn
		   (if (equal env-name "nil") (setq env-name nil))
		   (if (equal env-name "t") (setq env-name t))
		   (format
		    "(OO-Browser):  Environment `%s' is built; browse it now? "
		    (or (and (stringp env-name) env-name)
			(file-name-nondirectory env-file)))))
		;;
		;; Kill any buffers attached to Env files that will have been
		;; overwritten by the background build.  This avoids
		;; any `file changed on disk messages'.
		(env-buffer (get-file-buffer env-file))
		(env-ftrs-buffer (get-file-buffer
				  (br-feature-tags-file-name env-file)))
		(env-tags-buffer (get-file-buffer
				  (expand-file-name
				   "TAGS" (file-name-directory env-file)))))
	   (if env-buffer (kill-buffer env-buffer))
	   (if env-ftrs-buffer (kill-buffer env-ftrs-buffer))
	   (if env-tags-buffer (kill-buffer env-tags-buffer))
	   ;;
	   (cond ((and (br-in-browser)
		       (equal env-file br-env-file))
		  ;; Since we are in the browser under the same Env as just
		  ;; built, reload it without prompting the user.
		  (br-env-load env-file env-name nil t)
		  (message
		   "(OO-Browser):  Environment `%s' built and reloaded successfully"
		  (or (and (stringp env-name) env-name)
		      (file-name-nondirectory br-env-file))))
		 ((y-or-n-p prompt)
		  (br-quit t)
		  ;; The quit above forces a reload of the Environment here.
		  (setq br-env-file env-file
			br-env-name env-name)
		  (br-init env-file) ;; initializes auxiliary Env file variables
		  (br-env-browse env-file env-name)
		  (br-refresh)
		  )
		 ((equal env-file br-env-file)
		  ;; Ensure that new Env settings are loaded for the next
		  ;; time the browser is invoked.
		  (br-quit t)
		  (br-env-load env-file env-name nil t)
		  (message "(OO-Browser):  Reloaded Environment `%s' for later usage."
			   (or (and (stringp env-name) env-name)
			       (file-name-nondirectory br-env-file)))))))))

(defun br-env-cond-build (env-file env-name prompt)
  "Build current Environment from its specification and save it in ENV-FILE.
ENV-NAME is used with PROMPT to prompt user before building the Environment.
Return t iff current Environment gets built from specification.  Do not load
the Environment after building."
  (let ((dir (or (file-name-directory env-file)
		 default-directory)))
    (if (not (file-writable-p dir))
	(progn (beep)
	       (message "Unwritable Environment directory, \"%s\"" dir)
	       (sit-for 4) nil)
      (if (or (not prompt)
	      (y-or-n-p (format prompt
				(or (and (stringp env-name) env-name)
				    env-file))))
	  (progn (br-env-build env-file env-name 'prompt t) t)))))

(defun br-env-copy (to-br)
  "Copy `br-' Environment to or from `br-lang-prefix' language variables.
If TO-BR is non-nil, copy from language-specific variables to browser
variables.  Otherwise, do copy in the reverse direction."
  (let* ((var1) (var2)
	 (copy-func
	  (if to-br (function
		     (lambda ()
		       (if (boundp var2) (set var1 (symbol-value var2)))))
	    (function (lambda ()
			(if (boundp var1) (set var2 (symbol-value var1))))))))
    (mapcar (function
	      (lambda (nm)
	       (setq var1 (intern (concat "br-" nm))
		     var2 (intern (concat br-lang-prefix nm)))
	       (funcall copy-func)))
	    (append
	      '("env-file" "env-name" "env-version"
		"env-start-build-time" "env-end-build-time"
		"lib-search-dirs"
		"lib-prev-search-dirs" "lib-parents-htable"
		"lib-paths-htable" "sys-search-dirs"
		"sys-prev-search-dirs" "sys-parents-htable"
		"sys-paths-htable" "paths-htable" "parents-htable")
	      br-env-single-vars))))

(defun br-env-create-alists ()
  "Create all empty Environment association lists."
  (setq br-children-alist    nil
	br-sys-paths-alist   nil  br-lib-paths-alist nil
	br-sys-parents-alist nil  br-lib-parents-alist nil
	br-paths-alist       nil  br-parents-alist nil
	br-features-alist    nil  br-feature-paths-alist nil))

(defun br-env-create-htables ()
  "Create all empty Environment hash tables."
  (setq br-children-htable (hash-make 0)
	br-sys-paths-htable (hash-make 0)
	br-sys-parents-htable (hash-make 0)
	br-lib-paths-htable (hash-make 0)
	br-lib-parents-htable (hash-make 0)
	br-paths-htable (hash-make 0)
	br-parents-htable (hash-make 0)
	br-features-htable (hash-make 0)
	br-feature-paths-htable (hash-make 0)))

(defun br-env-default-file (&optional directory)
  "Search up current or optional DIRECTORY tree for an OO-Browser environment file.
Return file name found, the value of `br-env-file' if non-nil, or else the
value of `br-env-default-file'.  All return values are expanded to absolute
paths before being returned."
  (let ((path directory)
	(oo-browser-file))
    (while (and (stringp path)
		(setq path (file-name-directory path))
		(setq path (directory-file-name path))
		;; Not at root directory
		(not (string-match ":?/\\'" path))
		;; No environment file
		(not (file-exists-p
		      (setq oo-browser-file (expand-file-name
				       br-env-default-file path)))))
      (setq oo-browser-file nil))
    (expand-file-name (or oo-browser-file br-env-file br-env-default-file))))

(defun br-env-edit (env-file)
  "Read in ENV-FILE for editing and disable undo and backups within it."
  (prog1 (set-buffer (funcall br-find-file-noselect-function env-file))
    (buffer-disable-undo (current-buffer))
    (make-local-variable 'make-backup-files)
    (make-local-variable 'backup-inhibited)
    (setq make-backup-files nil
	  backup-inhibited t
	  buffer-read-only nil)))

(defun br-env-file-sym-val (symbol-name)
  "Given a SYMBOL-NAME, a string, find its value in the current Environment file.
Only search for the SYMBOL-NAME from the current point in the buffer.
Return cons whose car is t iff SYMBOL-NAME was found and then whose cdr is the
non-quoted value found."
  (set-buffer (funcall br-find-file-noselect-function br-env-file))
  (save-excursion
    (if (search-forward symbol-name nil t)
	(let ((standard-input (current-buffer)))
	  (cons t (eval (read)))))))

(defun br-env-try-load (env-file default-file)
  "Try to load a complete Environment, initially given by ENV-FILE.
If an Environment specification is selected, the user will be prompted
whether or not to build it.  If ENV-FILE is not a string, the function will
prompt for an Environment to load.  DEFAULT-FILE is the default file to use
when an empty value is given at the Environment file prompt.

Return the name of the Environment file that was loaded or nil."
  (if (stringp env-file)
	nil
    (if (stringp default-file)
	nil
      (setq default-file (br-env-default-file)))
    (setq env-file (br-env-read-file-name
		    "OO-Browser Environment to load: "
		    default-file default-file t)))
  (br-env-load env-file nil 'prompt nil))

(defun br-env-get-dirs (prompt)
  "PROMPT for and return list of directory names.
PROMPT must contain a %d somewhere in it, so dir # may be inserted."
  (let ((dir) (dirs) (num 1) (default ""))
    (while (not (string-equal
		 "" (setq dir (read-file-name
			       (format prompt num) default default t))))
      (if (file-directory-p dir)
	  (setq dirs (cons (file-name-as-directory dir) dirs)
		num (1+ num)
		default "")
	(beep)
	(setq default dir)))
    (nreverse dirs)))

(defun br-env-init (env-file same-lang same-env)
  "Load or build ENV-FILE if non-nil.
Otherwise, use `br-env-file' if non-nil or if not, interactively prompt for
Environment name.  SAME-LANG should be non-nil if invoking the OO-Browser on
the same language again.  SAME-ENV should be non-nil if invoking the
OO-Browser on the same Environment again.  br-sys/lib-search-dirs variables
should be set before this function is called.

Return the name of the current Environment file unless load attempt fails,
then return nil."
  (cond 

   ;; Specific environment requested
   (env-file
    ;; Create or load spec and load or build Environment
    (setq env-file (br-env-try-load env-file br-env-file)))
    
   ;; First invocation on this lang
   ((and (null br-sys-search-dirs) (null br-lib-search-dirs))
    ;; Create or load spec and load or build Environment
    (setq env-file
	  (br-env-try-load (or br-env-file (br-env-create)) br-env-file)))
    
   ;; Non-first invocation, code search paths have been set, possibly default Env
   (t
    (setq env-file br-env-file)
    (cond
     ;; Continue browsing an Environment
     (same-env nil)
     (same-lang
      ;; But code search paths have changed, so rebuild Env
      (or (eq br-sys-search-dirs br-sys-prev-search-dirs)
	  (br-build-sys-htable))
      (or (eq br-lib-search-dirs br-lib-prev-search-dirs)
	  (br-build-lib-htable)))
     ;; Request to browse a different language Env
     (t
      (setq env-file (br-env-try-load
		      (or br-env-file (br-env-create)) br-env-file))))))
  ;; Return current Env file name unless load attempt failed, then return nil.
  env-file)

(defun *br-env-internal-structures* ()
  "Display values of internal data structures in viewer buffer."
  (interactive)
  (br-funcall-in-view-window
   (concat br-buffer-prefix-info "Info*")
   (function
    (lambda ()
      (let ((standard-output (current-buffer)))
	(mapcar
	 (function
	  (lambda (sym)
	    (mapcar
	     (function (lambda (obj)
			 (princ obj)))
	     (list "!!! " (symbol-name sym) " !!!\n\n" 
		   (symbol-value sym) "\n\n"))
	    ))
	 '(br-children-htable
	   br-parents-htable
	   br-paths-htable
	   br-features-htable
	   br-feature-paths-htable
	   br-sys-search-dirs
	   br-sys-paths-htable
	   br-sys-parents-htable
	   br-lib-search-dirs
	   br-lib-paths-htable
	   br-lib-parents-htable
	   br-lang-prefix
	   br-env-spec)))))))

(defun br-env-lang-dialog-box (dialog-box)
  "Prompt user with DIALOG-BOX and return selected value.
Assumes caller has checked that `dialog-box' function exists."
  (let ((echo-keystrokes 0)
	event-obj
	event)	 
    ;; Add a cancel button to dialog box.
    (setq dialog-box (append dialog-box (list nil '["Cancel" abort t])))
    (popup-dialog-box dialog-box)
    (catch 'br-env-done
      (while t
	(setq event (next-command-event event)
	      event-obj (event-object event))
	(cond ((and (menu-event-p event)
		    (memq event-obj '(abort menu-no-selection-hook)))
	       (signal 'quit nil))
	      ((button-release-event-p event) ;; don't beep twice
	       nil)
	      ((menu-event-p event)
	       (throw 'br-env-done (eval event-obj)))
	      (t
	       (beep)
	       (message "Please answer the dialog box.")))))))

(defun br-env-lang-var (lang-prefix)
  "Create language-specific Environment variables for LANG-PREFIX."
  (eval (list 'defvar (intern (concat lang-prefix "env-version"))
	      nil
	      "Version of the OO-Browser used to build the current Environment or nil."))
  (eval (list 'defvar (intern (concat lang-prefix "env-file"))
	      br-env-default-file
	      "*File in which to save Environment.")))

(defun br-env-load-matching-htables (changed-types-list)
  (let ((still-changed-types))
    (if (file-readable-p br-env-file)
	(unwind-protect
	    (progn
	      (let ((buf (get-file-buffer br-env-file)))
		(and buf (kill-buffer buf)))
	      (set-buffer (funcall br-find-file-noselect-function br-env-file))
	      (goto-char (point-min))
	      (mapcar
		(function
		  (lambda (type)
		    (let* ((search-dirs (concat "br-" type "-search-dirs"))
			   (prev-dirs (concat "br-" type "-prev-search-dirs"))
			   (paths (concat "br-" type "-paths-htable"))
			   (parents (concat "br-" type "-parents-htable"))
			   (dirs-val (cdr (br-env-file-sym-val search-dirs))))
		      (if (equal dirs-val (symbol-value (intern search-dirs)))
			  (and (br-member type changed-types-list)
			       (progn (set (intern paths)
					   (cdr (br-env-file-sym-val paths)))
				      (set (intern parents)
					   (cdr (br-env-file-sym-val parents)))
				      (set (intern prev-dirs)
					   (symbol-value
					     (intern search-dirs)))))
			(setq still-changed-types
			      (cons type still-changed-types)))))) 
		'("sys" "lib"))
	      )
	  nil))
    (nreverse still-changed-types)))

(defun br-env-read-file-name (prompt &optional dir default must-match)
  "Read file name, prompting with PROMPT and completing in directory DIR.
Beep and re-prompt if a directory name is given rather than a file name.
The file name read is processed by `substitute-in-file-name' but is
not expanded (call `expand-file-name' for this).

Default name to DEFAULT if user enters a null string.  (If DEFAULT is
omitted, the visited file name is used.)  Fourth arg MUST-MATCH non-nil
means require existing file's name.  Non-nil and non-t means also require
confirmation after completion.  DIR defaults to current buffer's directory
default."
  (or dir (setq dir (file-name-directory (or default (br-env-default-file)))))
  (or default (setq default (br-env-default-file)))
  (if noninteractive
      default
    (let ((env default))
      (while (and (setq env (read-file-name
			     prompt
			     (br-env-substitute-home env) env must-match))
		  (file-directory-p env))
	(beep))
      env)))

(defun br-env-read-language-prefix (env-file)
  (save-excursion
    (set-buffer (find-file-noselect env-file))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (if (search-forward "br-lang-prefix" nil t)
	  (progn (forward-line 1)
		 ;; Eval removes quote from in front of lang-string
		 ;; value which is read from the Env file.
		 (eval (read (current-buffer))))))))

(defun br-env-save-mult-vars (mult-vars)
  (let ((br-sym))
    (mapcar
      (function
	(lambda (suffix)
	  (mapcar
	    (function
	      (lambda (type-str)
		(setq br-sym (intern-soft
			       (concat "br-" type-str suffix)))
		(if (and br-sym (boundp br-sym))
		    (let* ((nm (symbol-name br-sym))
			   (nm-mid (string-match "-htable$" nm)))
		      (if nm-mid
			  (progn (terpri) (princ (substring nm 0 nm-mid))
				 (princ "-alist\n'")
				 (hash-prin1 (symbol-value br-sym)))
			(terpri) (princ br-sym) (princ "\n'")
			(prin1 (symbol-value br-sym))
			(terpri))))))
	    '("sys-" "lib-"))))
      mult-vars)))

(defun br-env-set-htables (&optional skip-children)
  (br-env-add-ref-classes "lib")
  (br-env-add-ref-classes "sys")
  ;; Make System entries override Library entries which they duplicate, since
  ;; this is generally more desirable than merging the two.  Don't do this
  ;; for the paths-htable, however, since the value is the union of both
  ;; values.
  (br-merge-paths-htables)
  (br-merge-parents-htables)
  (if skip-children nil (br-build-children-htable)))

(defun br-env-select-lang ()
  "Interactively select and return value for `br-lang-prefix'."
  (let ((n 0) (nlangs (length br-env-lang-avector))
	(lang-prompt)
	;; Use dialog box if last user event involved the mouse.
	(use-dialog-box (and (fboundp 'popup-dialog-box)
			     (fboundp 'button-press-event-p)
			     (or (button-press-event-p last-command-event)
				 (button-release-event-p last-command-event)
				 (menu-event-p last-command-event)))))
    ;; Create a prompt numbering each OO-Browser language available.
    (setq lang-prompt
	  (if use-dialog-box
	      (mapcar
	       (function (lambda (lang)
			   (setq n (1+ n))
			   (vector lang (list 'identity n) 't)))
	       (mapcar 'car br-env-lang-avector))
	    (mapconcat
	     (function (lambda (lang)
			 (setq n (1+ n))
			 (format "%d\) %s" n lang)))
	     (mapcar 'car br-env-lang-avector)
	     "; ")))
    ;; Prompt user.
    (while (progn
	     (setq n (if use-dialog-box
			 (br-env-lang-dialog-box
			  (cons "Choose language to browse: " lang-prompt))
		       ;; Otherwise, prompt in the minibuffer.
		       (read-number (concat lang-prompt ": ") t)))
	     (or (< n 1) (> n nlangs)))
      (beep))
    (cdr (aref br-env-lang-avector (1- n)))))

(defun br-env-totals-minibuffer ()
  "Return a one line string of Environment class totals."
  (let ((sys (length (br-all-classes "sys")))
	(lib (length (br-all-classes "lib"))))
    (format "Total unique classes: %d  (System: %d; Library: %d)"
	    (+ sys lib) sys lib)))

(defun br-env-totals ()
  "Return string of Environment class totals."
  (let* ((sys-class-list (br-all-classes "sys"))
	 (lib-class-list (br-all-classes "lib"))
	 (sys-classes (length sys-class-list))
	 (lib-classes (length lib-class-list))
	 (duplicates (car (br-all-classes nil t)))
	 (sys-interfaces 0) (lib-interfaces 0)
	 sys-default-classes lib-default-classes
	 count)
    (if (br-interface-support-p)
	(setq sys-interfaces
	      (length (delq nil (mapcar 'br-interface-p sys-class-list)))
	      lib-interfaces
	      (length (delq nil (mapcar 'br-interface-p lib-class-list)))))
    (setq sys-default-classes
	  (length (delq nil (mapcar 'br-default-class-p sys-class-list)))
	  lib-default-classes
	  (length (delq nil (mapcar 'br-default-class-p lib-class-list))))
    (concat
     (format
"%s                       System    Library    Subtotals
------------------------------------------------------
Unique Env Classes:    %6d       %4d        %5d
Default Classes:       %6d       %4d        %5d\n"
       (if (null duplicates)
	   ""
	 (setq count (length duplicates))
	 (format "%d DUPLICATE CLASS%s TO CONSIDER ELIMINATING:\n\t%s\n\n"
		 count (if (= count 1) "" "ES") duplicates))
       ;; Unique Env Classes
       (- sys-classes sys-default-classes sys-interfaces)
       (- lib-classes lib-default-classes lib-interfaces)
       (+ (- sys-classes sys-default-classes sys-interfaces)
	  (- lib-classes lib-default-classes lib-interfaces))
       ;; Default Classes
       sys-default-classes lib-default-classes 
       (+ sys-default-classes lib-default-classes))
     ;;
     (if (br-interface-support-p)
	 (format
"Interfaces:            %6d       %4d        %5d\n"
           ;; Interfaces
           sys-interfaces lib-interfaces (+ sys-interfaces lib-interfaces)))
     ;; Totals
     (format
"------------------------------------------------------
            Totals:    %6d       %4d        %5d"
         sys-classes lib-classes (+ sys-classes lib-classes )))))

(defun br-env-validate-arg-strings (caller-name env-file env-name)
  "Called from CALLER-NAME function, validate and set ENV-FILE and ENV-NAME.
Return a cons of these two values." 
  (br-names-initialize)
  (if (and (stringp env-name) (null env-file))
      (setq env-file (br-name-get-env-file env-name)))
  (if (stringp env-file)
      (progn (if (string-match "-FTR$" env-file)
		 (setq env-file (substring env-file 0 (match-beginning 0))))
	     (setq env-file (expand-file-name env-file))
	     (if (null env-name)
		 (setq env-name (br-name-get env-file)))
	     (let ((env-directory))
	       (if (not (file-exists-p
			 (setq env-directory
			       (file-name-directory
				(expand-file-name env-file)))))
		   ;; Directory containing filename no longer exists.
		   ;; Trigger error.
		   (error "(%s): %s's directory, \"%s\" no longer exists."
			  caller-name env-name env-directory))))
    (error "(%s): Invalid Env file: `%s'" 
	   caller-name env-file))
  (if (null env-name)
; Old code that could improperly attach wrong an old env name to a new env file.
;      (if br-env-name
;	  (setq env-name br-env-name)
;	(setq env-name (br-name-add nil env-file)))
      (setq env-name (br-name-add nil env-file)))
  (cons env-file env-name))

;;; ************************************************************************
;;; Internal variables
;;; ************************************************************************

(defvar br-env-start-build-time nil
  "The time at which the last build of the current Environment was started or nil.")

(defvar br-env-end-build-time nil
  "The time at which the last build of the current Environment finished or nil.")

(defvar br-env-version nil
  "Version of the OO-Browser used to build the current Environment or nil.")

(defconst br-env-mult-vars
  '("search-dirs" "paths-htable" "parents-htable")
  "Descriptors of multiple copy variables saved as part of an Environment.")
(defconst br-env-single-vars
  '("lang-prefix" "env-spec" "children-htable" "features-htable"
    "feature-paths-htable")
  "Descriptors of singular variables saved as part of an Environment.")

(defvar br-env-spec nil
  "Non-nil value means Environment specification has been given but not yet built.
Nil means current Environment has been built, though it may still require
updating.  The value is language-specific.")

(defvar br-env-lang-avector
  '[("C++/C"   . "c++-")
    ("Eiffel"  . "eif-")
    ("Info"    . "info-")
    ("Java"    . "java-")
    ("Lisp"    . "clos-")
    ("Obj-C"   . "objc-")
    ("Python"  . "python-")
    ("Smalltalk" . "smt-")]
  "Association vector of (LANGUAGE-NAME-ABBREV . LANGUAGE-PREFIX-STRING) elements of OO-Browser languages.")

(defvar br-env-lang-name-alist
  '(("c++-" . "C++")
    ("eif-" . "Eiffel")
    ("info-" . "Info")
    ("java-" . "Java")
    ("clos-" . "CLOS")
    ("objc-" . "Objective-C")
    ("python-" . "Python")
    ("smt-" . "Smalltalk"))
  "Association list of (LANGUAGE-PREFIX-STRING . LANGUAGE-NAME) elements of OO-Browser languages.")

(mapcar 'br-env-lang-var (mapcar 'cdr br-env-lang-avector))

(provide 'br-env)
