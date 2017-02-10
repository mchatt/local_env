;;!emacs
;;
;; FILE:         hpath.el
;; SUMMARY:      Hyperbole support routines for handling UNIX paths.  
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     comm, hypermedia, unix
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:     1-Nov-91 at 00:44:23
;; LAST-MOD:     21-Apr-01 at 11:52:38 by Bob Weiner
;;
;; Copyright (C) 1991-1998  BeOpen.com
;; See the HY-COPY (Hyperbole) or BR-COPY (OO-Browser) file for license
;; information.
;;
;; This file is part of Hyperbole and the OO-Browser.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hversion) ;; for `hyperb:window-system' definition

(if (not (fboundp 'br-in-browser))
    ;; Then the OO-Browser is not loaded, so we can never be within the
    ;; browser.  Define this as a dummy function that always returns nil
    ;; until the OO-Browser is ever loaded.
    (defun br-in-browser ()
      "Always returns nil since the OO-Browser is not loaded."
      nil))

;;; ************************************************************************
;;; FILE VIEWER COMMAND SETTINGS
;;; ************************************************************************

(defvar hpath:display-alist
  (let ((info-suffix "\\.info\\(-[0-9]+\\)?\\(\\.gz\\|\\.Z\\|-z\\)?$"))
    (delq
     nil
     (list

      ;; Support internal sound when available.
      (if (fboundp 'play-sound-file)
	  '("\\.au$" . play-sound-file))

      ;; Run the OO-Browser on OOBR or OOBR-FTR Environment files.
      '("\\(\\`\\|/\\)\\(OOBR\\|oobr\\).*\\(-FTR\\|-ftr\\)?\\'" . br-env-browse)

      ;; Display the top node from Info online manuals.
      (cons
       (concat (` (, info-suffix))
	       "\\|/\\(info\\|INFO\\)/[^.]+$\\|/\\(info-local\\|INFO-LOCAL\\)/[^.]+$")
       (` (lambda (file)
	    (if (and (string-match (, info-suffix) file)
		     (match-beginning 1))
		;; Removed numbered trailer to get basic filename.
		(setq file (concat (substring file 0 (match-beginning 1))
				   (substring file (match-end 1)))))
	    (require 'info)
	    ;; Ensure that *info* buffer is displayed in the right place.
	    (hpath:display-buffer (current-buffer))
	    (condition-case ()
		(Info-find-node file "Top")
	      (error (if (and file (file-exists-p file))
			 (progn
			   (if (get-buffer "*info*")
			       (kill-buffer "*info*"))
			   (Info-find-node file "*" nil t))
		       (error "Invalid file")))))))

      '("\\.rdb\\'" . rdb:initialize)
      )))
  "*Alist of (FILENAME-REGEXP . EDIT-FUNCTION) elements for calling special
functions to display particular file types within Emacs.  See also
`hpath:file-alist' for external display program settings.")

(defvar hpath:display-buffer-alist
  (list
   (list 'this-window   'switch-to-buffer)
   (list 'other-window  (function (lambda (b)
				    (if (br-in-browser)
					(progn (br-to-view-window)
					       (switch-to-buffer b))
				      (switch-to-buffer-other-window b)))))
   (list 'one-window    (function (lambda (b)
				    (if (br-in-browser) (br-quit))
				    (delete-other-windows)
				    (switch-to-buffer b))))
   (list 'new-frame     (function (lambda (b)
				    (select-frame (make-frame))
				    (switch-to-buffer b))))
   (list 'other-frame   'hpath:display-buffer-other-frame)
   (list 'other-frame-one-window
	 (function (lambda (b)
		     (hpath:display-buffer-other-frame b)
		     (delete-other-windows)))))
  "*Alist of (DISPLAY-WHERE-SYMBOL  DISPLAY-BUFFER-FUNCTION) elements.
This permits fine-grained control of where Hyperbole displays linked to buffers.
The default value of DISPLAY-WHERE-SYMBOL is given by `hpath:display-where'.
Valid DISPLAY-WHERE-SYMBOLs are:
    this-window             - display in the current window
    other-window            - display in another window in the current frame
    one-window              - display in the current window, deleting other windows
    new-frame               - display in a new frame
    other-frame             - display in another, possibly existing, frame
    other-frame-one-window  - display in another frame, deleting other windows.")

(defvar hpath:display-where 'other-window
  "Symbol specifying the default method to use to display Hyperbole link referents.
See documentation of `hpath:display-where-alist' for valid values.")

(defvar hpath:display-where-alist
  (list
   (list 'this-window 'find-file)
   (list 'other-window (function (lambda (f)
				   (if (br-in-browser)
				       (progn (br-to-view-window)
					      (find-file f))
				     (find-file-other-window f)))))
   (list 'one-window  (function (lambda (f)
				  (if (br-in-browser) (br-quit))
				  (delete-other-windows) (find-file f))))
   (list 'new-frame   (function (lambda (f)
				  (if (fboundp 'find-file-new-frame)
				      (find-file-new-frame f)
				    (hpath:find-other-frame f)))))
   (list 'other-frame 'hpath:find-other-frame)
   (list 'other-frame-one-window
	 (function (lambda (f) (hpath:find-other-frame f) (delete-other-windows)))))
  "*Alist of (DISPLAY-WHERE-SYMBOL DISPLAY-FILE-FUNCTION) elements.
This permits fine-grained control of where Hyperbole displays linked to files.
The default value of DISPLAY-WHERE-SYMBOL is given by `hpath:display-where'.
Valid DISPLAY-WHERE-SYMBOLs are:
    this-window             - display in the current window
    other-window            - display in another window in the current frame
    one-window              - display in the current window, deleting other windows
    new-frame               - display in a new frame
    other-frame             - display in another, possibly existing, frame
    other-frame-one-window  - display in another frame, deleting other windows.")

;; `hyperb:window-system' variable from "hversion.el" must be defined
;; prior to this variable definition.
;;
(defvar hpath:find-alist
  (let ((nextstep-suffixes '(("\\.\\(adaptor\\|app\\|bshlf\\|clr\\|concur\\|create\\|diagram\\|dp\\|e?ps\\|frame\\|gif\\|locus\\|Mesa\\|nib\\|project\\|rtf\\|sense\\|tiff\\|tree\\)$" . "open")))
	(x-suffixes '(("\\.e?ps$" . "ghostview")
		      ("\\.dvi$"  . "xdvi")
		      ("\\.pdf$"  . ("xpdf" "acroread"))
		      ("\\.ps\\.g?[zZ]$" . "zcat %s | ghostview -")
		      ("\\.\\(gif\\|tiff?\\|xpm\\|xbm\\|xwd\\|pm\\|pbm\\|jpe?g\\)"  . "xv")
		      ("\\.ra?s$" . "snapshot -l")
		      ("\\.\\(fm\\|frame\\|mif\\)$" .
		       "frame.pl -vn -preader -c -f%s") ;; was "msgfm_driver"
		      ("\\.\\(doc\\|boo\\)$" . "ileaf")
		      )))
    (if (memq window-system '(dps ns))
	nextstep-suffixes
      (cdr (assoc hyperb:window-system
		  (list (cons "emacs19" x-suffixes)  ; GNU Emacs V19 under X
			(cons "xemacs"  x-suffixes)  ; XEmacs under X
			(cons "xterm"   x-suffixes)  ; GNU Emacs V18 under X
			(cons "next" nextstep-suffixes)
			'("apollo"  . nil)       ; Display Manager
			)))))
  "*Alist of (FILENAME-REGEXP . DISPLAY-PROGRAM-LIST) elements for using
window system dependent external programs to display particular file types.
The cdr of each element may be an executable name, a list of executable names 
\(the first valid one is used), or a function of one filename argument.
See also  `hpath:display-alist' for internal, window-system independent display
settings.")

;;; ************************************************************************
;;; LINK PATH VARIABLE SUBSTITUTION SETTINGS
;;; ************************************************************************

;; The following variable permits sharing of links over wide areas, where
;; links may contain variable references whose values may differ between
;; link creator and link activator.
;;
;; When a link is created, if its path contains a match for any of the
;; variable values in hpath:variables, then the variable's symbol is
;; substituted for the literal value.  Hyperbole then replaces the variable
;; with a matching value when the link is resolved.
;;
(defvar hpath:variables
  '(hyperb:dir Info-directory Info-directory-list sm-directory load-path exec-path)
  "*List of Emacs Lisp variable symbols to substitute within matching link paths.
Each variable value, if bound, must be either a pathname or a list of pathnames.")

;;; ************************************************************************
;;; Other public variables
;;; ************************************************************************

(defvar hpath:rfc "/anonymous@ftp.faqs.org:rfc/rfc%s.txt"
  "*String to be used in the call: (hpath:rfc rfc-num)
to create an path to the RFC document for `rfc-num'.")

(defvar hpath:suffixes '(".gz" ".Z")
  "*List of filename suffixes to add or remove within (hpath:exists-p) calls.")

(defvar hpath:tmp-prefix "/tmp/remote-"
  "*Pathname prefix to attach to remote files copied locally for use with external viewers.")

;; WWW URL format:  [URL[:=]]<protocol>:/[<user>@]<domain>[:<port>][/<path>]
;;             or   [URL[:=]]<protocol>://[<user>@]<domain>[:<port>][<path>]
;; Avoid [a-z]:/path patterns since these may be disk paths on OS/2, DOS or
;; Windows.
(defvar hpath:url-regexp "<?\\(URL[:=]\\)?\\(\\([a-zA-Z][a-zA-Z]+\\)://?/?\\([^/:@ \t\n\r\"`'|]+@\\)?\\([^/:@ \t\n\r\"`'|]+\\)\\(\\)\\(:[0-9]+\\)?\\([/~]\\([^\]\[@ \t\n\r\"`'|(){}<>]+[^\]\[@ \t\n\r\"`'|(){}<>.,?#!*]\\)*\\)?\\)>?"
  "Regular expression which matches a Url in a string or buffer.
Its match groupings and their names are:
  1 = hpath:url-keyword-grpn = optional `URL:' or `URL=' literal
  2 = hpath:url-grpn         = the whole URL
  3 = hpath:protocol-grpn    = access protocol
  4 = hpath:username-grpn    = optional username
  5 = hpath:sitename-grpn    = URL site to connect to
  6 = unused                 = for compatibility with hpath:url-regexp2
  7 = hpath:portnumber-grpn  = optional port number to use
  8 = hpath:pathname-grpn    = optional pathname to access.")

(defvar hpath:url-hostnames-regexp  "\\(www\\|ftp\\|gopher\\|wais\\|telnet\\|news\\|nntp\\)"
  "Grouped regexp alternatives of hostnames that automatically determine the Url access protocol to use.")

(defvar hpath:url-regexp2
  (concat
   "<?\\(URL[:=]\\|[^/@]\\|\\)\\(\\(\\)\\(\\)\\("
   hpath:url-hostnames-regexp
   "\\.[^/:@ \t\n\r\"`'|]+\\)\\(:[0-9]+\\)?\\([/~]\\([^\]\[@ \t\n\r\"`'|(){}<>]+[^\]\[@ \t\n\r\"`'|(){}<>.,?#!*]\\)*\\)?\\)>?")
  "Regular expression which matches a Url in a string or buffer.
Its match groupings and their names are:
  1 = hpath:url-keyword-grpn = optional `URL:' or `URL=' literal
  2 = hpath:url-grpn         = the whole URL
  3 = unused                 = for compatibility with hpath:url-regexp
  4 = unused                 = for compatibility with hpath:url-regexp
  5 = hpath:sitename-grpn    = URL site to connect to
  6 = hpath:hostname-grpn    = hostname used to determine the access protocol, e.g. ftp.domain.com
  7 = hpath:portnumber-grpn  = optional port number to use
  8 = hpath:pathname-grpn    = optional pathname to access.")

(defconst hpath:url-keyword-grpn 1
  "Optional `URL:' or `URL=' literal.  See doc for `hpath:url-regexp' and `hpath:url-regexp2'.")
(defconst hpath:url-grpn 2
  "The whole URL.  See doc for `hpath:url-regexp' and `hpath:url-regexp2'.")
(defconst hpath:protocol-grpn 3
  "Access protocol.  See doc for `hpath:url-regexp' and `hpath:url-regexp2'.")
(defconst hpath:username-grpn 4
  "Optional username.  See doc for `hpath:url-regexp' and `hpath:url-regexp2'.")
(defconst hpath:sitename-grpn 5
  "URL site to connect to.  See doc for `hpath:url-regexp' and `hpath:url-regexp2'.")
(defconst hpath:hostname-grpn 6
  "Hostname used to determine the access protocol, e.g. ftp.domain.com.
See doc for `hpath:url-regexp' and `hpath:url-regexp2'.")
(defconst hpath:portnumber-grpn 7
  "Optional port number to use.  See doc for `hpath:url-regexp' and `hpath:url-regexp2'.")
(defconst hpath:pathname-grpn 8
  "Optional pathname to access.  See doc for `hpath:url-regexp' and `hpath:url-regexp2'.")

(defvar hpath:string-url-regexp (concat "\\`" hpath:url-regexp "\\'")
  "Regular expression that matches to a string that contains a possibly delimited Url and nothing else.
See the documentation for `hpath:url-regexp' for match groupings to
use with string-match.")

(defvar hpath:string-url-regexp2 (concat "\\`" hpath:url-regexp2 "\\'")
  "Regular expression that matches to a string that contains a possibly delimited terse Url and nothing else.
See the documentation for `hpath:url-regexp' for match groupings to
use with string-match.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hpath:absolute-to (path &optional default-dirs)
  "Returns PATH as an absolute path relative to one directory from optional DEFAULT-DIRS or `default-directory'.
Returns PATH unchanged when it is not a valid path or when DEFAULT-DIRS
is invalid.  DEFAULT-DIRS when non-nil may be a single directory or a list of
directories.  The first one in which PATH is found is used."
  (if (not (and (stringp path) (hpath:is-p path nil t)))
      path
    (if (not (cond ((null default-dirs)
		    (setq default-dirs (cons default-directory nil)))
		   ((stringp default-dirs)
		    (setq default-dirs (cons default-dirs nil)))
		   ((listp default-dirs))
		   (t nil)))
	path
      (let ((rtn) dir)
	(while (and default-dirs (null rtn))
	  (setq dir (expand-file-name
		     (file-name-as-directory (car default-dirs)))
		rtn (expand-file-name path dir)
		default-dirs (cdr default-dirs))
	  (or (file-exists-p rtn) (setq rtn nil)))
	(or rtn path)))))

(defun hpath:efs-at-p ()
  "Returns an efs pathname that point is within or nil.
See the `efs' Emacs lisp package for pathname format details.
Always returns nil if (hpath:efs-available-p) returns nil."
  (if (hpath:efs-available-p)
      (let ((user (hpath:efs-default-user))
	    path)
	(setq path
	      (save-excursion
		(skip-chars-backward "^[ \t\n\r\f\"`'|\(\{<")
		(cond
		  ((looking-at hpath:url-regexp)
		   (if (string-equal
			(buffer-substring (match-beginning hpath:protocol-grpn)
					  (match-end hpath:protocol-grpn))
			"ftp")
		       (concat
			"/"
			;; user
			(if (match-beginning hpath:username-grpn)
			    (buffer-substring (match-beginning
					       hpath:username-grpn)
					      (match-end hpath:username-grpn))
			  (concat user "@"))
			;; sitename
			(hpath:delete-trailer
			 (buffer-substring (match-beginning hpath:sitename-grpn)
					   (match-end hpath:sitename-grpn)))
			":"
			;; path
			(if (match-beginning hpath:pathname-grpn)
			    (buffer-substring (match-beginning hpath:pathname-grpn)
					      (match-end hpath:pathname-grpn))))
		     ;; else ignore this other type of WWW path
		     ))
		  ((looking-at hpath:url-regexp2)
		   (if (string-equal
			(buffer-substring (match-beginning hpath:hostname-grpn)
					  (match-end hpath:hostname-grpn))
			"ftp")
		       (concat
			"/" user "@"
			;; site
			(hpath:delete-trailer
			 (buffer-substring (match-beginning hpath:sitename-grpn)
					   (match-end hpath:sitename-grpn)))
			":"
			;; path
			(if (match-beginning hpath:pathname-grpn)
			    (buffer-substring (match-beginning hpath:pathname-grpn)
					      (match-end hpath:pathname-grpn))))
		     ;; else ignore this other type of WWW path
		     ))
		  ;; user, site and path
		  ((looking-at "/?[^/:@ \t\n\r\"`'|]+@[^/:@ \t\n\r\"`'|]+:[^]@ \t\n\r\"`'|\)\}]*")
		   (buffer-substring (match-beginning 0) (match-end 0)))
		  ;; @site and path
		  ((looking-at "@[^/:@ \t\n\r\"`'|]+:[^]@ \t\n\r\"`'|\)\}]*")
		   (concat "/" user (buffer-substring
				      (match-beginning 0) (match-end 0))))
		  ;; site and path
		  ((and (looking-at
			  "/?\\(\\([^/:@ \t\n\r\"`'|]+\\):[^]@:, \t\n\r\"`'|\)\}]*\\)[] \t\n\r,.\"`'|\)\}]")
			(setq path (buffer-substring
				     (match-beginning 1) (match-end 1)))
			(string-match "[^.]\\.[^.]"
				      (buffer-substring (match-beginning 2)
							(match-end 2))))
		   (concat "/" user "@" path))
		  ;; host and path
		  ((and (looking-at
			 "/\\([^/:@ \t\n\r\"`'|]+:[^]@:, \t\n\r\"`'|\)\}]*\\)")
			(setq path (buffer-substring
				     (match-beginning 1) (match-end 1))))
		   (concat "/" user "@" path))
		  )))
	(hpath:delete-trailer path))))

(defun hpath:efs-p (path)
  "Returns non-nil iff PATH is an efs pathname.
See the `efs' Emacs lisp package for pathname format details.
Always returns nil if (hpath:efs-available-p) returns nil."
  (and (stringp path)
       (or (featurep 'efs) (featurep 'ange-ftp))
       (let ((user (hpath:efs-default-user))
	     result)
	 (setq result
	       (cond
		((string-match hpath:string-url-regexp path)
		 (if (string-equal
		      (substring path (match-beginning hpath:protocol-grpn)
				 (match-end hpath:protocol-grpn))
		      "ftp")
		     (concat
		      "/"
		      ;; user
		      (if (match-beginning hpath:username-grpn)
			  (substring path (match-beginning hpath:username-grpn)
				     (match-end hpath:username-grpn))
			(concat user "@"))
		      ;; site
		      (hpath:delete-trailer
		       (substring path (match-beginning hpath:sitename-grpn)
				  (match-end hpath:sitename-grpn)))
		      ":"
		      ;; path
		      (if (match-beginning hpath:pathname-grpn)
			  (substring path (match-beginning hpath:pathname-grpn)
				     (match-end hpath:pathname-grpn))))
		   ;; else ignore this other type of WWW path
		   ))
		((string-match hpath:string-url-regexp2 path)
		 (if (string-equal
		      (substring path (match-beginning hpath:hostname-grpn)
				 (match-end hpath:hostname-grpn))
		      "ftp")
		     (concat
		      "/" user "@"
		      ;; site
		      (hpath:delete-trailer
		       (substring path (match-beginning hpath:sitename-grpn)
				  (match-end hpath:sitename-grpn)))
		      ":"
		      ;; path
		      (if (match-beginning hpath:pathname-grpn)
			  (substring path (match-beginning hpath:pathname-grpn)
				     (match-end hpath:pathname-grpn))))
		   ;; else ignore this other type of WWW path
		   ))
		 ;; user, site and path
		 ((string-match "/?[^/:@ \t\n\r\"`'|]+@[^/:@ \t\n\r\"`'|]+:[^]@ \t\n\r\"`'|\)\}]*"
				path)
		  (substring path (match-beginning 0) (match-end 0)))
		 ;; @site and path
		 ((string-match "@[^/:@ \t\n\r\"`'|]+:[^]@ \t\n\r\"`'|\)\}]*"
				path)
		  (concat "/" user
			  (substring path (match-beginning 0) (match-end 0))))
		 ;; site and path
		 ((and (string-match
			 "/?\\(\\([^/:@ \t\n\r\"`'|]+\\):[^]@:, \t\n\r\"`'|\)\}]*\\)"
			 path)
		       (setq result (substring path
					       (match-beginning 1) (match-end 1)))
		       (string-match "[^.]\\.[^.]"
				     (substring path (match-beginning 2)
						(match-end 2))))
		  (concat "/" user "@" result))
		 ;; host and path
		 ((and (string-match
			 "/\\([^/:@ \t\n\r\"`'|]+:[^]@:, \t\n\r\"`'|\)\}]*\\)"
			 path)
		       (setq result (substring
				      path
				      (match-beginning 1) (match-end 1))))
		  (concat "/" user "@" result))
		 ))
	(hpath:delete-trailer result))))

(defun hpath:at-p (&optional type non-exist)
  "Returns delimited path or non-delimited efs path at point, if any.
World-Wide Web urls are ignored and therefore dealt with by other code.
Delimiters may be: double quotes, open and close single quote, whitespace, or
Texinfo file references.  If optional TYPE is the symbol 'file or 'directory,
then only that path type is accepted as a match.  Only locally reachable
paths are checked for existence.  With optional NON-EXIST, nonexistent local
paths are allowed.  Absolute pathnames must begin with a `/' or `~'."
  (cond
   ;; Local file URLs
   ;; ((hpath:is-p (hargs:delimited "file://" "[ \t\n\r\"\'\}]" nil t)))
   ((hpath:efs-at-p))
   ((hpath:www-at-p) nil)
   ((hpath:is-p (or (hargs:delimited "\"" "\"") 
		    ;; Filenames in Info docs or Python files
		    (hargs:delimited "[\`\']" "\'" t)
		    ;; Filenames in TexInfo docs
		    (hargs:delimited "@file{" "}")
		    ;; Any existing whitespace delimited filename at point.
		    (and (not non-exist)
			 (hargs:delimited "^\\|\\(\\s \\|[\]\[(){}<>\;&,]\\)*"
					  "\\([\]\[(){}<>\;&,]\\|\\s \\)+\\|$"
					  t t)))
		type non-exist))))

(defun hpath:display-buffer (buffer &optional display-where)
  "Displays BUFFER at optional DISPLAY-WHERE location or at hpath:display-where.
BUFFER may be a buffer or a buffer name.

See documentation of `hpath:display-buffer-alist' for valid values of DISPLAY-WHERE.
Returns non-nil iff buffer is actually displayed."
  (interactive "bDisplay buffer: ")
  (if (stringp buffer) (setq buffer (get-buffer buffer)))
  (if (null buffer)
      nil
    (hpath:push-tag-mark)
    (if (null display-where)
	(setq display-where hpath:display-where))
    (funcall (car (cdr (or (assq display-where
				 hpath:display-buffer-alist)
			   (assq 'other-window
				 hpath:display-buffer-alist))))
	     buffer)
    t))

(defun hpath:display-buffer-other-frame (buffer)
  "Displays BUFFER, in another frame.
May create a new frame, or reuse an existing one.
See documentation of `hpath:display-buffer' for details.
Returns the dispalyed buffer."
  (interactive "bDisplay buffer in other frame: ")
  (hpath:push-tag-mark)
  (if (= (length (frame-list)) 1)
      (select-frame (make-frame))
    (other-frame 1))
  (if (br-in-browser)
      (br-to-view-window))
  (switch-to-buffer buffer))

(defun hpath:find (filename &optional display-where)
  "Edits file FILENAME using user customizable settings of display program and location.

FILENAME may start with a special prefix character which is
handled as follows:
  !filename  - execute as a non-windowed program within a shell;
  &filename  - execute as a windowed program;
  -filename  - load as an Emacs Lisp program.

Otherwise, if FILENAME matches a regular expression in the variable
`hpath:find-alist,' the associated external display program is invoked.
If not, `hpath:display-alist' is consulted for a specialized internal
display function to use.  If no matches are found there,
`hpath:display-where-alist' is consulted using the optional argument,
DISPLAY-WHERE (a symbol) or if that is nil, the value of
`hpath:display-where', and the matching display function is used.
Returns non-nil iff file is displayed within a buffer (not with an external
program)."
  (interactive "FFind file: ")
  (let (modifier)
    (if (string-match hpath:prefix-regexp filename)
	(setq modifier (aref filename 0)
	      filename (substring filename (match-end 0))))
    (setq filename (hpath:substitute-value filename))
    (or (file-exists-p filename)
	(error "(hpath:find): \"%s\" does not exist"
	       (file-relative-name filename)))
    (or (file-readable-p filename)
	(error "(hpath:find): \"%s\" is not readable"
	       (file-relative-name filename)))
    (let (efs-filename)
      ;; If filename is a remote file (not a directory, we have to copy it to
      ;; a temporary local file and then display that.
      (if (and (setq efs-filename (hpath:efs-p filename))
	       (not (file-directory-p efs-filename)))
	  (copy-file efs-filename
		     (setq filename
			   (concat hpath:tmp-prefix
				   (file-name-nondirectory efs-filename)))
		     t t)))
    (cond (modifier (cond ((eq modifier ?!)
			   (hact 'exec-shell-cmd filename))
			  ((eq modifier ?&)
			   (hact 'exec-window-cmd filename))
			  ((eq modifier ?-)
			   (load filename)))
		    nil)
	  (t (let ((display-executables (hpath:find-program filename))
		   executable)
	       (cond ((stringp display-executables)
		      (hact 'exec-window-cmd 
			    (hpath:command-string display-executables
						  filename))
		      nil)
		     ((hypb:functionp display-executables)
		      (funcall display-executables filename)
		      t)
		     ((and (listp display-executables) display-executables)
		      (setq executable (hpath:find-executable
					display-executables))
		      (if executable
			  (hact 'exec-window-cmd
				(hpath:command-string executable
						      filename))
			(error "(hpath:find): No available executable from: %s"
			       display-executables)))
		     (t (setq filename (hpath:validate filename))
			(if (null display-where)
			    (setq display-where hpath:display-where))
			(funcall
			 (car (cdr (or (assq display-where
					     hpath:display-where-alist)
				       (assq 'other-window
					     hpath:display-where-alist))))
			 filename)
			t)))))))

(defun hpath:find-executable (executable-list)
  "Return the first executable string from EXECUTABLE-LIST found within `exec-path'."
  (catch 'found
    (mapcar
     (function
      (lambda (executable)
	(if (stringp executable)
	    ;; Match only to files with execute permission.
	    (if (locate-file executable exec-path nil 1)
		(throw 'found executable))
	  (error "(hpath:find-executable): Non-string entry, %s"
		 executable-list))))
     executable-list)
    nil))

(defun hpath:find-line (filename line-num &optional display-where)
  "Edits file FILENAME with point placed at LINE-NUM.

`hpath:display-where-alist' is consulted using the optional argument,
DISPLAY-WHERE (a symbol) or if that is nil, the value of
`hpath:display-where', and the matching display function is used to determine
where to display the file, e.g. in another frame.
Always returns t."
  (interactive "FFind file: ")
  ;; Just delete any special characters preceding the filename, ignoring them.
  (if (string-match hpath:prefix-regexp filename)
      (setq filename (substring filename (match-end 0))))
  (setq filename (hpath:substitute-value filename)
	filename (hpath:validate filename))
  (if (null display-where)
      (setq display-where hpath:display-where))
  (funcall (car (cdr (or (assq display-where
			       hpath:display-where-alist)
			 (assq 'other-window
			       hpath:display-where-alist))))
	   filename)
  (if (integerp line-num)
      (progn (widen) (goto-line line-num)))
  t)

(defun hpath:find-other-frame (filename)
  "Edits file FILENAME, in another frame.
May create a new frame, or reuse an existing one.
See documentation of `hpath:find' for details.
Returns the buffer of displayed file."
  (interactive "FFind file in other frame: ")
  (if (= (length (frame-list)) 1)
      (if (fboundp 'id-create-frame)
	  (id-create-frame)
	(select-frame (make-frame)))
    (other-frame 1))
  (if (br-in-browser)
      (br-to-view-window))
  (find-file filename))

(defun hpath:find-other-window (filename)
  "Edits file FILENAME, in another window or using an external program.
May create a new window, or reuse an existing one; see the function display-buffer.
See documentation of `hpath:find' for details.
Returns non-nil iff file is displayed within a buffer."
  (interactive "FFind file in other window: ")
  (hpath:find filename 'other-window))

(defun hpath:is-p (path &optional type non-exist)
  "Returns PATH if PATH is a Unix path, else nil.
If optional TYPE is the symbol 'file or 'directory, then only that path type
is accepted as a match.  The existence of the path is checked only for
locally reachable paths (Info paths are not checked).  Single spaces are
permitted in the middle of existing pathnames, but not at the start or end.
Tabs and newlines are converted to space before the pathname is checked, this
normalized path form is what is returned for PATH.  With optional NON-EXIST,
nonexistent local paths are allowed."
  (let ((rtn-path path)
	(suffix))
    (and (stringp path)
	 ;; Path may be a link reference with other components other than a
	 ;; pathname.  These components always follow a comma or # symbol, so
	 ;; strip them, if any, before checking path.
	 (if (string-match "[ \t\n\r]*," path)
	     (setq rtn-path (concat (substring path 0 (match-beginning 0))
				     "%s" (substring path (match-beginning 0)))
		   path (substring path 0 (match-beginning 0)))
	   (setq rtn-path (concat rtn-path "%s")))
	 ;; If path is just an local HTML reference that begins with #,
	 ;; prepend the file name to it.
	 (cond ((and buffer-file-name
		     ;; ignore HTML color strings
		     (not (string-match "\\`#[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]\\'" path))
		     ;; match to in-file HTML references
		     (string-match "\\`#[^#]+\\'" path))
		(setq rtn-path (concat "file://" buffer-file-name rtn-path)
		      path buffer-file-name))
	       ((string-match "\\`[^#]+\\(#[^#]+\\)\\'" path)
		;; file and # reference
		(setq path (substring path 0 (match-beginning 1)))
		(if (memq (aref path 0) '(?/ ?~))
		    ;; absolute
		    (setq rtn-path (concat "file://" rtn-path))
		  (setq path (concat default-directory path)
			rtn-path (concat "file://" default-directory rtn-path))))
	       (t))
	 (if (string-match hpath:prefix-regexp path)
	     (setq path (substring path (match-end 0)))
	   t)
	 (not (or (string-equal path "")
		  (string-match "\\`\\s \\|\\s \\'" path)))
	 ;; Convert tabs and newlines to space.
	 (setq path (hbut:key-to-label (hbut:label-to-key path)))
	 (or (not (string-match "[()]" path))
	     (string-match "\\`([^ \t\n\r\)]+)[ *A-Za-z0-9]" path))
	 (if (string-match "\\$\{[^\}]+}" path)
	     (setq path (hpath:substitute-value path))
	   t)
	 (not (string-match "[\t\n\r\"`'|{}\\]" path))
	 (or (not (hpath:www-p path))
	     (string-match "\\`ftp[:.]" path))
	 (let ((remote-path (string-match "@.+:\\|^/.+:\\|.+:/" path)))
	   (if (cond (remote-path
		      (cond ((eq type 'file)
			     (not (string-equal "/" (substring path -1))))
			    ((eq type 'directory)
			     (string-equal "/" (substring path -1)))
			    (t)))
		     ((or (and non-exist
			       (or
				;; Info or efs path, so don't check for.
				(string-match "[()]" path)
				(hpath:efs-p path)
				(setq suffix (hpath:exists-p path t))
				;; Don't allow spaces in non-existent
				;; pathnames.
				(not (string-match " " path))))
			  (setq suffix (hpath:exists-p path t)))
		      (cond ((eq type 'file)
			     (not (file-directory-p path)))
			    ((eq type 'directory)
			     (file-directory-p path))
			    (t)))
		     )
	       (progn
		 ;; Quote any but last %s within rtn-path.
		 (setq rtn-path (hypb:replace-match-string "%s" rtn-path "%%s")
		       rtn-path (hypb:replace-match-string "%%s\\'" rtn-path "%s"))
		 ;; Return path if non-nil return value.
		 (if (stringp suffix);; suffix could = t, which we ignore
		     (if (string-match
			  (concat (regexp-quote suffix) "%s") rtn-path)
			 ;; remove suffix
			 (concat (substring rtn-path 0 (match-beginning 0))
				 (substring rtn-path (match-end 0)))
		       ;; add suffix
		       (format rtn-path suffix))
		   (format rtn-path ""))))))))

(defun hpath:push-tag-mark ()
  "Add a tag return marker at point if within a programming language file buffer.
Is a no-op if the function `push-tag-mark' is not available."
  (if (and buffer-file-name
	   comment-start
	   (fboundp 'push-tag-mark)
	   (not (memq last-command
		      '(find-tag find-tag-other-window tags-loop-continue))))
      ;; push old position
      (push-tag-mark)))

(defun hpath:relative-to (path &optional default-dir)
  "Returns PATH relative to optional DEFAULT-DIR or `default-directory'.
Returns PATH unchanged when it is not a valid path."
  (if (not (and (stringp path) (hpath:is-p path)))
      path
    (setq default-dir
	  (expand-file-name
	   (file-name-as-directory (or default-dir default-directory)))
	  path (expand-file-name path))
    (and path default-dir
	 (if (string-equal "/" default-dir)
	     path
	   (let ((end-dir (min (length path) (length default-dir))))
	     (cond
	      ((string-equal (substring path 0 end-dir) default-dir)
	       (concat "./" (substring path end-dir)))
	      ((progn (setq default-dir (file-name-directory (directory-file-name default-dir))
			    end-dir (min (length path) (length default-dir)))
		      (string-equal (substring path 0 end-dir) default-dir))
	       (concat "../" (substring path end-dir)))
	      ((progn (setq default-dir (file-name-directory (directory-file-name default-dir))
			    end-dir (min (length path) (length default-dir)))
		      (string-equal (substring path 0 end-dir) default-dir))
	       (concat "../../" (substring path end-dir)))
	      (t path)))))))

(defun hpath:rfc (rfc-num)
  "Return pathname to textual rfc document indexed by RFC-NUM.
See the documentation of the `hpath:rfc' variable."
  (format hpath:rfc rfc-num))

(defun hpath:substitute-value (path)
  "Substitutes matching value for Emacs Lisp variables and environment variables in PATH.
Returns path with variable values substituted."
  (substitute-in-file-name
    (hypb:replace-match-string
      "\\$\{[^\}]+}"
      path
      (function
	(lambda (str)
	  (let* ((var-group (substring path match start))
		 (var-name (substring path (+ match 2) (1- start)))
		 (rest-of-path (substring path start))
		 (sym (intern-soft var-name)))
	    (if (file-name-absolute-p rest-of-path)
		(setq rest-of-path (substring rest-of-path 1)))
	    (if (and sym (boundp sym))
		(directory-file-name
		 (hpath:substitute-dir var-name rest-of-path))
	      var-group))))
      t)))

(defun hpath:substitute-var (path)
  "Replaces up to one match in PATH with first matching variable from `hpath:variables'."
  (if (not (and (stringp path) (string-match "/" path) (hpath:is-p path)))
      path
    (setq path (hpath:symlink-referent path))
    (let ((new-path)
	  (vars hpath:variables)	  
	  result var val)
      (while (and vars (null new-path))
	(setq var (car vars) vars (cdr vars))
	(if (boundp var)
	    (progn (setq val (symbol-value var))
		   (cond ((stringp val)
			  (if (setq result
				    (hpath:substitute-var-name var val path))
			      (setq new-path result)))
			 ((null val))
			 ((listp val)
			  (while (and val (null new-path))
			    (if (setq result
				    (hpath:substitute-var-name var (car val) path))
				(setq new-path result))
			    (setq val (cdr val))))
			 (t (error "(hpath:substitute-var): `%s' has invalid value for hpath:variables" var))))))
      (or new-path path)
      )))

;;
;; The following function recursively resolves all UNIX links to their
;; final referents.
;; Works with Apollo's variant and other strange links like:
;; /usr/local -> $(SERVER_LOCAL)/usr/local, /usr/bin ->
;; ../$(SYSTYPE)/usr/bin and /tmp -> `node_data/tmp.  It also handles
;; relative links properly as in /usr/local/emacs -> gnu/emacs which must
;; be resolved relative to the `/usr/local' directory.
;; It will fail on Apollos if the `../' notation is used to move just
;; above the `/' directory level.  This is fairly uncommon and so the
;; problem has not been fixed.
;;
(defun hpath:symlink-referent (linkname)
  "Returns expanded file or directory referent of LINKNAME.
LINKNAME should not end with a directory delimiter.
Returns nil if LINKNAME is not a string.
Returns LINKNAME unchanged if it is not a symbolic link but is a pathname."
  (if (stringp linkname)
      (or (file-symlink-p linkname) linkname)))

(defun hpath:symlink-expand (referent dirname)
  "Returns expanded file or directory REFERENT relative to DIRNAME."
  (let ((var-link)
	(dir dirname))
    (while (string-match "\\$(\\([^\)]*\\))" referent)
      (setq var-link (getenv (substring referent (match-beginning 1)
					(match-end 1)))
	    referent (concat (substring referent 0 (match-beginning 0))
			     var-link
			     (substring referent (match-end 0)))))
    ;; If referent is not an absolute path
    (let ((nd-abbrev (string-match "`node_data" referent)))
      (if (and nd-abbrev (= nd-abbrev 0))
	  (setq referent (concat
			   ;; Prepend node name given in dirname, if any
			   (and (string-match "^//[^/]+" dirname)
				(substring dirname 0 (match-end 0)))
			   "/sys/" (substring referent 1)))))
    (while (string-match "\\(^\\|/\\)\\.\\.\\(/\\|$\\)" referent)
      ;; Match to "//.." or "/.." at the start of link referent
      (while (string-match "^\\(//\\.\\.\\|/\\.\\.\\)\\(/\\|$\\)" referent)
	(setq referent (substring referent (match-end 1))))
      ;; Match to "../" or ".." at the start of link referent
      (while (string-match "^\\.\\.\\(/\\|$\\)" referent)
	(setq dir (file-name-directory (directory-file-name dir))
	      referent (concat dir (substring referent (match-end 0)))))
      ;; Match to rest of "../" in link referent
      (while (string-match "[^/]+/\\.\\./" referent)
	(setq referent (concat (substring referent 0 (match-beginning 0))
			       (substring referent (match-end 0))))))
    (and (not (eq (aref referent 0) ?~))
	 (not (eq (aref referent 0) ?/))
	 (setq referent (expand-file-name referent dirname))))
  referent)

(defun hpath:validate (path)
  "Returns PATH if PATH is a valid, readable path, else signals error.
Info and efs remote pathnames are considered readable without any
validation checks.
Default-directory should be equal to current Hyperbole button source
directory when called, so that PATH is expanded relative to it." 
  (cond ((not (stringp path))
	 (error "(hpath:validate): \"%s\" is not a pathname." path))
	((or (string-match "[()]" path) (hpath:efs-p path))
	 ;; info or efs path, so don't validate
	 path)
	((if (not (hpath:www-p path))
	     ;; Otherwise, must not be a WWW link ref and must be a readable
	     ;; path.
	     (let ((return-path (hpath:exists-p path)))
	       (and return-path (file-readable-p return-path)
		    return-path))))
	(t (error "(hpath:validate): \"%s\" is not readable." path))))

(defun hpath:url-at-p ()
  "Return world-wide-web universal resource locator (url) that point immediately precedes or nil.
See the documentation for `hpath:url-regexp' for match groupings to
use with buffer-substring."
  (if (or (looking-at hpath:url-regexp) (looking-at hpath:url-regexp2))
      (save-excursion
	(goto-char (match-end hpath:url-grpn))
	(skip-chars-backward ".,?#!*()")
	(buffer-substring (match-beginning hpath:url-grpn) (point)))))

(defun hpath:url-p (obj)
  "Return t if OBJ is a world-wide-web universal resource locator (url) string, else nil.
See the documentation for `hpath:url-regexp' for match groupings to
use with string-match."
  (and (stringp obj)
       (or (string-match hpath:string-url-regexp obj)
	   (string-match hpath:string-url-regexp2 obj))
       t))

(defun hpath:www-at-p (&optional include-start-and-end-p)
  "Returns a world-wide-web link reference that point is within or nil.
With optional INCLUDE-START-AND-END-P non-nil, returns list of:
  (link-string begin-position end-position)."
  (save-excursion
    (skip-chars-backward "^\[ \t\n\r\f\"`'|\(\{<")
    (cond ((not include-start-and-end-p)
	   (hpath:url-at-p))
	  ((or (looking-at hpath:url-regexp) (looking-at hpath:url-regexp2))
	   (goto-char (match-end hpath:url-grpn))
	   (skip-chars-backward ".,?#!*()")
	   (list (buffer-substring (match-beginning hpath:url-grpn) (point))
		 (match-beginning hpath:url-grpn)
		 (point))))))

(defun hpath:www-p (path)
  "Returns PATH iff PATH is a world-wide-web link reference, else nil."
  (and (stringp path) (hpath:url-p path) path))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hpath:command-string (cmd filename)
  "Return a single string that runs a shell CMD over FILENAME.
CMD may contain a single `%s' indicating where FILENAME is to
be integrated, otherwise the filename is appended as an argument."
  ;; Permit %s substitution of filename within program.
  (if (string-match "[^%]%s" cmd)
      (format cmd filename)
    (concat cmd " " filename)))

(defun hpath:efs-available-p ()
  "Return t if the efs package is available, nil otherwise.
Either the package must have been loaded already or under versions of Emacs
19, it must be set for autoloading via `file-name-handler-alist'."
  (or (featurep 'efs) (featurep 'ange-ftp)
      (and (boundp 'file-name-handler-alist) ; v19
	   (or (rassq 'efs-file-handler-function file-name-handler-alist)
	       (rassq 'ange-ftp-hook-function file-name-handler-alist))
	   t)))

(defun hpath:efs-default-user ()
  "Return default user account for remote file access with efs or ange-ftp.
Returns \"anonymous\" if neither `efs-default-user' nor `ange-ftp-default-user'
is set."
  (cond ((and (boundp 'efs-default-user)
	      (stringp efs-default-user))
	 efs-default-user)
	((and (boundp 'ange-ftp-default-user)
	      (stringp ange-ftp-default-user))
	 ange-ftp-default-user)
	(t "anonymous")))

(defun hpath:delete-trailer (string)
  "Return string minus any trailing .?#!*() characters."
  (save-match-data
    (if (and (stringp string) (> (length string) 0)
	     (string-match "[.?#!*()]+\\'" string))
	(substring string 0 (match-beginning 0))
      string)))

(defun hpath:exists-p (path &optional suffix-flag)
  "Return PATH if it exists.  (This does not mean you can read it.)
If PATH exists with or without a suffix from hpath:suffixes, then that
pathname is returned.

With optional SUFFIX-FLAG and PATH exists, return suffix added or removed
from path or t."
  (let ((return-path)
	(suffix) suffixes)
    (if (file-exists-p path)
	(setq return-path path)
      (setq suffixes hpath:suffixes)
      (while suffixes
	(setq suffix (car suffixes))
	(if (string-match (concat (regexp-quote suffix) "\\'") path)
	    ;; Remove suffix
	    (setq return-path (substring path 0 (match-beginning 0)))
	  ;; Add suffix
	  (setq return-path (concat path suffix)))
	(if (file-exists-p return-path)
	    (setq suffixes nil);; found a match
	  (setq suffix nil
		suffixes (cdr suffixes)
		return-path nil))))
    (if return-path
	(if suffix-flag
	    (or suffix t)
	  return-path))))

(defun hpath:find-program (filename)
  "Return one or a list of shell or Lisp commands to execute to display FILENAME or nil.
Return nil if FILENAME is a directory name.
See also documentation for `hpath:find-alist' and `hpath:display-alist'."
  (cond ((and (stringp filename) (file-directory-p filename))
	 nil)
	((and (fboundp 'image-mode)
	      (string-match hpath:native-image-suffixes filename))
	 nil)
	((let ((case-fold-search t))
	   (hpath:match filename hpath:find-alist)))
	(t (let ((case-fold-search nil))
	     (hpath:match filename hpath:display-alist)))))

(defun hpath:match (filename regexp-alist)
  "If FILENAME matches the car of any element in REGEXP-ALIST, return its cdr.
REGEXP-ALIST elements must be of the form (<filename-regexp>
. <command-to-display-file>).  <command-to-display-file> may be a string
representing an external window-system command to run or it may be a Lisp
function to call with FILENAME as its single argument."
  (let ((cmd)
	elt)
    (while (and (not cmd) regexp-alist)
      (if (string-match (car (setq elt (car regexp-alist))) filename)
	  (setq cmd (cdr elt)))
      (setq regexp-alist (cdr regexp-alist)))
    cmd))

(defun hpath:substitute-dir (var-name rest-of-path)
  "Returns a dir for VAR-NAME using REST-OF-PATH to find match or triggers an error when no match.
VAR-NAME's value may be a directory or a list of directories.  If it is a
list, the first directory prepended to REST-OF-PATH which produces a valid
local pathname is returned."
  (let (sym val)
    (cond ((not (stringp var-name))
	   (error "(hpath:substitute-dir): VAR-NAME arg, `%s', must be a string" var-name))
	  ((not (and (setq sym (intern-soft var-name))
		     (boundp sym)))
	   (error "(hpath:substitute-dir): VAR-NAME arg, \"%s\", is not a bound variable"
		  var-name))
	  ((stringp (setq val (symbol-value sym)))
	   (if (hpath:validate (expand-file-name rest-of-path val))
	       val))
	  ((listp val)
	   (let ((dir))
	     (while (and val (not dir))
	       (setq dir (car val) val (cdr val))
	       (or (and (stringp dir)
			(file-name-absolute-p dir)
			(file-readable-p (expand-file-name rest-of-path dir)))
		   (setq dir nil)))
	     (if dir (hpath:validate (directory-file-name dir))
	       (error "(hpath:substitute-dir): Can't find match for \"%s\""
		      (concat "$\{" var-name "\}/" rest-of-path))
	       )))
	  (t (error "(hpath:substitute-dir): Value of VAR-NAME, \"%s\", must be a string or list" var-name))
	  )))

(defun hpath:substitute-var-name (var-symbol var-dir-val path)
  "Replaces with VAR-SYMBOL any occurrences of VAR-DIR-VAL in PATH.
Replacement is done iff VAR-DIR-VAL is an absolute path.
If PATH is modified, returns PATH, otherwise returns nil."
  (if (and (stringp var-dir-val) (file-name-absolute-p var-dir-val))
      (let ((new-path (hypb:replace-match-string
			(regexp-quote (file-name-as-directory
					(or var-dir-val default-directory)))
			path (concat "$\{" (symbol-name var-symbol) "\}/")
			t)))
	(if (equal new-path path) nil new-path))))


;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar hpath:native-image-suffixes "\\.\\(xpm\\|png\\|gif\\|jpe?g\\)\\'"
  "Regular expression matching file name suffixes of natively handled image types.
Used only if the function `image-mode' is defined.")

(defvar hpath:prefix-regexp "\\`[-!&][ ]*"
  "Regexp matching command characters which may precede a pathname.
These are used to indicate how to display or execute the pathname.
  - means evaluate it as Emacs Lisp;
  ! means execute it as a shell script
  & means run it under the current window system.")

(provide 'hpath)
