;;!emacs
;;
;; FILE:         hypb.el
;; SUMMARY:      Miscellaneous Hyperbole support features.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     extensions, hypermedia
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:     6-Oct-91 at 03:42:38
;; LAST-MOD:     13-Jul-99 at 00:00:59 by Bob Weiner
;;
;; Copyright (C) 1991-1995, 1997, 1998  BeOpen.com
;; See the HY-COPY (Hyperbole) or BR-COPY (OO-Browser) file for license
;; information.
;;
;; This file is part of Hyperbole and the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(mapcar 'require '(hversion hact))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defconst hypb:help-buf-prefix "*Help: Hyperbole "
  "Prefix attached to all native Hyperbole help buffer names.
This should end with a space.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hypb:call-process-p (program &optional infile predicate &rest args)
  "Calls an external PROGRAM with INFILE for input.
If PREDICATE is given, it is evaluated in a buffer with the PROGRAM's
output and the result returned.  If PREDICATE is nil, returns t iff
program has no output or just a 0-valued output.
Rest of ARGS are passed as arguments to PROGRAM."
  (let ((buf (get-buffer-create "*test-output*"))
	(found))
    (save-excursion
      (set-buffer buf) (setq buffer-read-only nil) (erase-buffer)
      (apply 'call-process program infile buf nil args)
      (setq found 
	    (if predicate
		(eval predicate)
	      (or (= (point-max) 1) ;; No output, consider cmd a success.
		  (and (< (point-max) 4)
		       (string= (buffer-substring 1 2) "0")))))
      (set-buffer-modified-p nil)
      (kill-buffer buf))
    found))


(defun hypb:chmod (op octal-permissions file)
  "Uses OP and OCTAL-PERMISSIONS integer to set FILE permissions.
OP may be +, -, xor, or default =."
  (let ((func (cond ((eq op '+)   (function logior))
		    ((eq op '-)   (function
				   (lambda (p1 p2) (logand (lognot p1) p2))))
		    ((eq op 'xor) (function logxor))
		    (t            (function (lambda (p1 p2) p1))))))
    (set-file-modes file (funcall func (hypb:oct-to-int octal-permissions)
				  (file-modes file)))))

(defun hypb:cmd-key-string (cmd-sym &optional keymap)
  "Returns a single pretty printed key sequence string bound to CMD-SYM.
Global keymap is used unless optional KEYMAP is given."
  (if (and cmd-sym (symbolp cmd-sym) (fboundp cmd-sym))
  (let* ((get-keys (function
		    (lambda (cmd-sym keymap)
		      (key-description (where-is-internal
					cmd-sym keymap 'first)))))
	 (keys (funcall get-keys cmd-sym keymap)))
    (concat "{"
	    (if (string= keys "")
		(concat (funcall get-keys 'execute-extended-command nil)
			" " (symbol-name cmd-sym) " RET")
	      keys)
	    "}"))
  (error "(hypb:cmd-key-string): Invalid cmd-sym arg: %s." cmd-sym)))

;;;###autoload
(defun hypb:configuration (&optional out-buf)
  "Insert Emacs configuration information at the end of optional OUT-BUF or the current buffer."
  (save-excursion
    (and out-buf (set-buffer out-buf))
    (goto-char (point-min))
    (if (re-search-forward mail-header-separator nil t)
	(forward-line 1)
      (goto-char (point-max)))
    (delete-blank-lines) (delete-blank-lines)
    (let ((start (point)))
      (insert (format "I use:\tEditor:      %s\n\tHyperbole:   %s\n"
		      (cond ((boundp 'infodock-version)
			     infodock-version)
			    (t (hypb:replace-match-string
				" of .+" (emacs-version) "" t)))
                      hyperb:version))
      (if (and (boundp 'system-configuration) (stringp system-configuration))
	  (insert (format "\tSys Type:    %s\n" system-configuration)))
      (insert (format "\tOS Type:     %s\n\tWindow Sys:  %s\n"
                      system-type (or window-system hyperb:window-system
				      "None")))
      (if (and (boundp 'hmail:reader) hmail:reader)
          (insert (format "\tMailer:      %s\n"
                          (cond ((eq hmail:reader 'rmail-mode) "RMAIL")
                                ((eq hmail:reader 'vm-mode)
                                 (concat "VM " vm-version))
                                ((and (eq hmail:reader 'mh-show-mode)
                                      (string-match "v ?\\([0-9]+.[0-9]+\\)"
                                          mh-e-RCS-id))
                                 (concat "MH-e "
                                         (substring mh-e-RCS-id
                                                    (match-beginning 1)
                                                    (match-end 1))))
                                ((eq hmail:reader 'pm-fdr-mode)
                                 (concat "PIEmail " pm-version))
                                ))))
      (if (and (boundp 'hnews:reader) (boundp 'gnus-version) hnews:reader)
          (insert (format "\tNews Rdr:    %s\n" gnus-version)))
      (if (and (boundp 'br-version) (stringp br-version))
	  (insert (format "\tOO-Browser:  %s\n" br-version)))
      (insert "\n")
      (untabify start (point)))))

(defun hypb:debug ()
  "Loads Hyperbole hbut.el source file and sets debugging traceback flag."
  (interactive)
  (or (featurep 'hinit) (load "hsite"))
  (or (and (featurep 'hbut)
	   (let ((func (hypb:indirect-function 'ebut:create)))
	     (not (or (hypb:v19-byte-code-p func)
		      (eq 'byte-code
			  (car (car (nthcdr 3 (hypb:indirect-function
					       'ebut:create)))))))))
      (load "hbut.el"))
  (setq debug-on-error t))

(defun hypb:domain-name ()
  "Returns current Internet domain name with '@' prepended or nil if none."
  (let* ((dname-cmd (or (file-exists-p "/usr/bin/domainname")
			(file-exists-p "/bin/domainname")))
	 (dname (or (getenv "DOMAINNAME")
		    (if dname-cmd
			(hypb:call-process-p
			 "domainname" nil 
			 '(substring (buffer-string) 0 -1))))))
    (if (or (and dname (string-match "\\." dname))
	    (let* ((src "/etc/resolv.conf")
		   (src-buf-exists-p (get-file-buffer src)))
	      (and (file-exists-p src) (file-readable-p src)
		   (with-temp-buffer
		     (insert-file-contents-literally src)
		     (goto-char (point-min))
		     (if (re-search-forward  "^domain[ \t]+\\([^ \t\n\r]+\\)"
					     nil t)
			 (setq dname (buffer-substring (match-beginning 1)
						       (match-end 1))))
		     (or src-buf-exists-p (kill-buffer nil))
		     dname))))
	(concat "@" dname))))

(defun hypb:error (&rest args)
  "Signals an error typically to be caught by 'hui:menu'."
  (let ((msg (apply 'format args)))
    (put 'error 'error-message msg)
    (error msg)))

(defun hypb:functionp (obj)
"Returns t if OBJ is a function, nil otherwise."
  (cond
    ((symbolp obj) (fboundp obj))
    ((subrp obj))
    ((hypb:v19-byte-code-p obj))
    ((consp obj)
     (if (eq (car obj) 'lambda) (listp (car (cdr obj)))))
    (t nil)))

(defun hypb:function-copy (func-symbol)
  "Copies FUNC-SYMBOL's body for overloading.  Returns copy of body."
  (if (fboundp func-symbol)
      (let ((func (hypb:indirect-function func-symbol)))
	(cond ((listp func) (copy-sequence func))
	      ((subrp func) (error "(hypb:function-copy): `%s' is a primitive; can't copy body."
				   func-symbol))
	      ((and (hypb:v19-byte-code-p func) (fboundp 'make-byte-code))
	       (if (not (fboundp 'compiled-function-arglist))
		   (let ((new-code (append func nil))) ; turn it into a list
		     (apply 'make-byte-code new-code))
		 ;; Can't reference bytecode objects as vectors in modern
		 ;; XEmacs.
		 (let ((new-code (nconc
				  (list (compiled-function-arglist func)
					(compiled-function-instructions func)
					(compiled-function-constants func)
					(compiled-function-stack-depth func)
					(compiled-function-doc-string func))))
		       spec)
		   (if (setq spec (compiled-function-interactive func))
		       (setq new-code (nconc new-code (list (nth 1 spec)))))
		   (apply 'make-byte-code new-code))))
	      (t (error "(hypb:function-copy): Can't copy function body: %s" func))))
    (error "(hypb:function-copy): `%s' symbol is not bound to a function."
	   func-symbol)))

(defun hypb:function-overload (func-sym prepend &rest new-forms)
  "Redefine function named FUNC-SYM by either PREPENDing (or appending if nil) rest of quoted NEW-FORMS."
  (let ((old-func-sym (intern
			(concat "*hypb-old-"
				(symbol-name func-sym)
				"*"))))
    (or (fboundp old-func-sym)
	(defalias old-func-sym (hypb:function-copy func-sym)))
    (let* ((old-func (hypb:indirect-function old-func-sym))
	   (old-param-list (action:params old-func))
	   (param-list (action:param-list old-func))
	   (old-func-call
	     (list (if (memq '&rest old-param-list)
		       ;; Have to account for extra list wrapper from &rest.
		       (cons 'apply
			     (cons (list 'quote old-func-sym) param-list))
		     (cons old-func-sym param-list)))))
      (eval (append
	      (list 'defun func-sym old-param-list)
	      (delq nil
		    (list
		      (documentation old-func-sym)
		      (action:commandp old-func-sym)))
	      (if prepend
		  (append new-forms old-func-call)
		(append old-func-call new-forms)))))))

(defun hypb:function-symbol-replace (func-sym sym-to-replace replace-with-sym)
  "Replaces in body of FUNC-SYM SYM-TO-REPLACE with REPLACE-WITH-SYM.
FUNC-SYM may be a function symbol or its body.  All occurrences within lists
are replaced.  Returns body of modified FUNC-SYM."
  (let ((body (hypb:indirect-function func-sym))
	(constant-vector) (constant))
    (if (listp body)
	;; assume V18 byte compiler
	(setq constant-vector
	      (car (delq nil (mapcar
			      (function
			       (lambda (elt)
				 (and (listp elt)
				      (vectorp (setq constant-vector (nth 2 elt)))
				      constant-vector)))
			      body))))
      ;; assume V19 byte compiler   (eq (compiled-function-p body) t)
      (setq constant (if (fboundp 'compiled-function-constants)
			 (compiled-function-constants body)
		       (aref body 2))
	    constant-vector (if (vectorp constant) constant)))
    (if constant-vector
	;; Code is byte-compiled.
	(hypb:constant-vector-symbol-replace
	 constant-vector sym-to-replace replace-with-sym)
      ;;
      ;; Code is not byte-compiled.
      ;; Replaces occurrence of symbol within lists only.
      (hypb:map-sublists
       (function
	(lambda (atom list)
	  ;; The ' in the next line *is* required for proper substitution.
	  (if (eq atom 'sym-to-replace)
	      (let ((again t))
		(while (and again list)
		  (if (eq (car list) atom)
		      (progn (setcar list replace-with-sym)
			     (setq again nil))
		    (setq list (cdr list))))))))
       body))
    body))

(defun hypb:help-buf-name (&optional suffix)
  "Returns a Hyperbole help buffer name for current buffer.
With optional SUFFIX string, uses it rather than buffer name."
  (let ((bn (or suffix (buffer-name))))
    (if (string-match (regexp-quote hypb:help-buf-prefix) bn)
	(buffer-name (generate-new-buffer bn))
      (concat hypb:help-buf-prefix bn "*"))))

(defun hypb:indirect-function (obj)
  "Return the function at the end of OBJ's function chain.
Resolves autoloadable function symbols properly."
  (let ((func
	 (if (fboundp 'indirect-function)
	     (indirect-function obj)
	   (while (symbolp obj)
	     (setq obj (symbol-function obj)))
	   obj)))
    ;; Handle functions with autoload bodies.
    (if (and (symbolp obj) (listp func) (eq (car func) 'autoload))
	(let ((load-file (car (cdr func))))
	  (load load-file)
	  ;; Prevent infinite recursion
	  (if (equal func (symbol-function obj))
	      (error "(hypb:indirect-function): Autoload of '%s' failed" obj)
	    (hypb:indirect-function obj)))
      func)))

(defun hypb:insert-region (buffer start end invisible-flag)
  "Insert into BUFFER the contents of a region from START to END in the current buffer.
INVISIBLE-FLAG, if non-nil, means invisible text in an outline region is
copied, otherwise, it is omitted."
  (let ((from-koutline (eq major-mode 'kotl-mode)))
  (append-to-buffer buffer start end)
  (save-excursion
    (set-buffer buffer)
    (let ((first (- (point) (- end start)))
	  (last (point)))
      ;; Remove from buffer any copied text that was hidden if invisible-flag
      ;; is nil.
      (if invisible-flag
	  ;; Show all hidden text within the copy.
	  (subst-char-in-region first last ?\r ?\n t)
	;; Remove hidden text.
	(goto-char first)
	(while (search-forward "\r" last t)
	  (delete-region (1- (point)) (progn (end-of-line) (point)))))
      ;;
      ;; If region came from a koutline, remove any characters with an
      ;; invisible property which separate cells.
      (if from-koutline
	  (kproperty:map
	   (function (lambda (prop) (delete-char 1))) 'invisible t))))))
	
(if (or hyperb:xemacs-p hyperb:emacs19-p)
    (defalias 'hypb:mark 'mark)
  (defun hypb:mark (inactive-p)
    "Return this buffer's mark value as integer, or nil if no mark.
INACTIVE-P non-nil means return value of mark even if region is not active
under Emacs version 19.
If you are using this in an editing command, you are most likely making
a mistake; see the documentation of `set-mark'."
    (mark))
  )
(if hyperb:xemacs-p
    (defalias 'hypb:mark-marker 'mark-marker)
  (defun hypb:mark-marker (inactive-p)
    "Return this buffer's mark as a marker object, or nil if no mark.
INACTIVE-P is unused, it is for compatibility with XEmacs' version of
mark-marker."
    (mark-marker))
  )

(defun hypb:map-sublists (func list)
  "Applies FUNC to every atom found at any level of LIST.
FUNC must take two arguments, an atom and a list in which the atom is found.
Returns values from applications of FUNC as a list with the same
structure as LIST.  FUNC is therefore normally used just for its side-effects."
  (mapcar (function (lambda (elt)
		      (if (atom elt)
			  (funcall func elt list)
			(hypb:map-sublists func elt))))
	  list))

(defun hypb:map-vector (func object)
  "Returns list of results of application of FUNC to each element of OBJECT.
OBJECT should be a vector or byte-code object."
  (if (not (or (vectorp object) (hypb:v19-byte-code-p object)))
      (error "(hypb:map-vector): Second argument must be a vector or byte-code object."))
  (let ((end (length object))
	(i 0)
	(result))
    (while (< i end)
      (setq result (cons (funcall func (aref object i)) result)
	    i (1+ i)))
    (nreverse result)))

(defun hypb:mouse-help-file ()
  "Return the full path to the Hyperbole mouse key help file."
  (cond ((and (fboundp 'locate-data-file)
	      (locate-data-file "hypb-mouse.txt")))
	(t (let* ((hypb-man (expand-file-name "man/" hyperb:dir))
		  (help-file (expand-file-name "hypb-mouse.txt" hypb-man)))
	     (if (or (file-exists-p help-file)
		     (file-exists-p
		      (setq help-file (expand-file-name
				       "hypb-mouse.txt" data-directory))))
		 help-file
	       (error "(hypb:mouse-help-file): Non-existent file: \"%s\""
		      help-file))))))

(if (or hyperb:xemacs-p hyperb:emacs19-p)
    (defalias 'hypb:push-mark 'push-mark)
  (defun hypb:push-mark (&optional location nomsg activate-region)
    "Set mark at LOCATION (point, by default) and push old mark on mark ring.
If the last global mark pushed was not in the current buffer,
also push LOCATION on the global mark ring.
Display `Mark set' unless the optional second arg NOMSG is non-nil.
Optional third arg ACTIVATE-REGION is ignored.

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  See the documentation of `set-mark' for more information."
    (push-mark location nomsg))
  )

(defun hypb:replace-match-string (regexp str newtext &optional literal)
  "Replaces all matches for REGEXP in STR with NEWTEXT string and returns the result.
Optional LITERAL non-nil means do a literal replacement.
Otherwise treat \\ in NEWTEXT string as special:
  \\& means substitute original matched text,
  \\N means substitute match for \(...\) number N,
  \\\\ means insert one \\.
NEWTEXT may instead be a function of one argument (the string to replace in)
that returns a replacement string."
  (if (not (stringp str))
      (error "(hypb:replace-match-string): 2nd arg must be a string: %s" str))
  (if (or (stringp newtext) (hypb:functionp newtext))
      nil
    (error "(hypb:replace-match-string): 3rd arg must be a string or function: %s"
	   newtext))
  (let ((rtn-str "")
	(start 0)
	(special)
	match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
	    start (match-end 0)
	    rtn-str
	    (concat
	      rtn-str
	      (substring str prev-start match)
	      (cond ((hypb:functionp newtext)
		     (hypb:replace-match-string
		      regexp (substring str match start)
		      (funcall newtext str) literal))
		    (literal newtext)
		    (t (mapconcat
			 (function
			   (lambda (c)
			     (if special
				 (progn
				   (setq special nil)
				   (cond ((eq c ?\\) "\\")
					 ((eq c ?&)
					  (substring str
						     (match-beginning 0)
						     (match-end 0)))
					 ((and (>= c ?0) (<= c ?9))
					  (if (> c (+ ?0 (length
							   (match-data))))
					      ;; Invalid match num
					      (error "(hypb:replace-match-string) Invalid match num: %c" c)
					    (setq c (- c ?0))
					    (substring str
						       (match-beginning c)
						       (match-end c))))
					 (t (char-to-string c))))
			       (if (eq c ?\\) (progn (setq special t) nil)
				 (char-to-string c)))))
			 newtext ""))))))
    (concat rtn-str (substring str start))))

(defun hypb:return-process-output (program &optional infile &rest args)
  "Returns as a string the output from external PROGRAM with INFILE for input.
Rest of ARGS are passed as arguments to PROGRAM.
Removes any trailing newline at the end of the output."
  (let ((buf (get-buffer-create "*test-output*"))
	(output))
    (save-excursion
      (set-buffer buf) (setq buffer-read-only nil) (erase-buffer)
      (apply 'call-process program infile buf nil args)
      (setq output (buffer-string))
      ;; Remove trailing newline from output.
      (if (> (length output) 0) (setq output (substring output 0 -1)))
      (set-buffer-modified-p nil)
      (kill-buffer buf))
    output))

(defun hypb:supercite-p ()
  "Returns non-nil iff the Emacs add-on supercite package is in use."
  (let (hook-val)
    (if (memq t (mapcar
		 (function
		  (lambda (hook-var)
		    (and (boundp hook-var)
			 (progn (setq hook-val (symbol-value hook-var))
				(cond ((listp hook-val)
				       (if (memq 'sc-cite-original hook-val)
					   t))
				      ((eq hook-val 'sc-cite-original)))))))
		 '(mail-citation-hook mail-yank-hooks)))
	t)))

(defun hypb:window-list (&optional minibuffer-flag)
  "Returns a list of Lisp window objects for all Emacs windows in selected frame.
Optional first arg MINIBUFFER-FLAG t means include the minibuffer window
in the list, even if it is not active.  If MINIBUFFER-FLAG is neither t
nor nil it means to not count the minibuffer window even if it is active."
  (let*	((initial-window (selected-window))
	 (window-list (list initial-window))
	 (window initial-window))
    (while (not (eq (setq window (next-window window minibuffer-flag))
		    initial-window))
      (setq window-list (cons window window-list)))
    window-list))

(defun hypb:v19-byte-code-p (obj)
  "Return non-nil iff OBJ is an Emacs V19 byte compiled object."
  (or (and (fboundp 'compiled-function-p) (compiled-function-p obj))
      (and (fboundp 'byte-code-function-p) (byte-code-function-p obj))))

;;; ************************************************************************
;;; About Hyperbole Setup
;;; ************************************************************************

;;;###autoload
(defun hypb:display-file-with-logo (&optional file)
  "Display an optional text FILE with the BeOpen.com banner prepended.
Without file, the banner is prepended to the current buffer."
  ;; 
  (if file
      ;; This function is defined in hversion.el when needed.
      (id-browse-file file))
  (if (or (not (fboundp 'make-glyph))
	  (let ((extent (next-extent (current-buffer))))
	    (and extent (extent-property extent 'beopen-banner))))
      ;; Either image support is unavailable or the image has already been
      ;; inserted, so don't reinsert it.
      nil
    (let* ((beopen-banner
	    (make-glyph (if (fboundp 'locate-data-file)
			    (locate-data-file "beopen-banner.xpm")
			  (expand-file-name "beopen-banner.xpm"
					    data-directory))))
	   (buffer-read-only)
	   extent)
      (goto-char (point-min))
      (insert "\n")
      (indent-to (startup-center-spaces beopen-banner))
      (insert "\n\n")
      (setq extent (make-extent (- (point) 3) (- (point) 2)))
      (set-extent-end-glyph extent beopen-banner)
      (set-extent-property extent 'beopen-banner t)
      (set-extent-property extent 'help-echo "Click to visit http://www.BeOpen.com.")
      (set-extent-property extent 'keymap hypb:beopen-banner-keymap))
    (goto-char (point-min))
    (skip-syntax-forward "-")
    (set-window-start (selected-window) 1)
    (set-buffer-modified-p nil)))

(defvar hypb:beopen-banner-keymap
  (let ((map (make-sparse-keymap)))
    (cond (hyperb:emacs19-p
	   (define-key map [button-1]  'hypb:beopen-home-page)
	   (define-key map [button-2]  'hypb:beopen-home-page)
	   (define-key map "\C-m"      'hypb:beopen-home-page))
	  (hyperb:xemacs-p
	   (define-key map 'button1  'hypb:beopen-home-page)
	   (define-key map 'button2  'hypb:beopen-home-page)
	   (define-key map '(return) 'hypb:beopen-home-page)))
    map)
  "Keymap used when on the BeOpen.com banner glyph.")

(defun hypb:beopen-home-page ()
  "Visit http://www.BeOpen.com."
  (interactive)
  (require 'hsys-w3)
  (hact 'www-url "http://www.beopen.com"))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hypb:constant-vector-symbol-replace
  (constant-vector sym-to-replace replace-with-sym)
  ;; Replace symbols within a byte-compiled constant vector.
  (let ((i (length constant-vector))
	constant)
    (while (>= (setq i (1- i)) 0)
      (setq constant (aref constant-vector i))
      (cond ((eq constant sym-to-replace)
	     (aset constant-vector i replace-with-sym))
	    ((and (fboundp 'compiled-function-p)
		  (compiled-function-p constant))
	     (hypb:function-symbol-replace
	      constant sym-to-replace replace-with-sym))))))

(defun hypb:oct-to-int (oct-num)
  "Returns octal integer OCTAL-NUM converted to a decimal integer."
  (let ((oct-str (int-to-string oct-num))
	(dec-num 0))
    (and (string-match "[^0-7]" oct-str)
	 (error "(hypb:oct-to-int): Bad octal number: %s" oct-str))
    (mapconcat (function
		(lambda (o)
		  (setq dec-num (+ (* dec-num 8)
				   (if (and (>= o ?0) (<= o ?7))
				       (- o ?0))))))
	       oct-str "")
    dec-num))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(provide 'hypb)
