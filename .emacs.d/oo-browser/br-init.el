;;!emacs
;;
;; FILE:         br-init.el
;; SUMMARY:      OO-Browser per Emacs session initialization.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:    18-May-90
;; LAST-MOD:      3-Aug-99 at 22:55:53 by Bob Weiner
;;
;; Copyright (C) 1990-1995, 1997, 1998 BeOpen.com
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Personal Variable Defaults
;;; ************************************************************************

;; >>> If you wish to edit classes displayed by the browser in an editor
;;     other than Emacs, set the `br-editor-cmd' variable to the command you
;;     want to edit with.  Arguments to the command should be placed in
;;     `br-ed[1-9], with one string argument per variable'.  Keep in
;;     mind that the command must generate a new window under your
;;     window system.  For example, to run vi under X, one needs to use the
;;     command line "xterm -e vi", the settings would then be:
;;
;;         (setq br-editor-cmd "xterm" br-ed1 "-e" 
;;               br-ed2 "vi")
;;
;;     This editor will only be used when the browser is run under a window
;;     system external to Emacs, like X.  (In such a case, the variable
;;     `hyperb:window-system' will be non-nil).
;;
;;
(defvar br-editor-cmd nil
  "When non-nil, the OO-Browser uses a non-standard command for editing files.
This may be either a string to invoke an external program or an Emacs
Lisp function which takes a single file argument.")

(setq br-ed1 nil br-ed2 nil br-ed3 nil br-ed4 nil br-ed5 nil
	br-ed6 nil br-ed7 nil br-ed8 nil br-ed9 nil)

;;
;; >>> If you want to view classes in a read-only fashion outside of Emacs,
;;     set the following `br-viewer-cmd' and `br-vw[1-9]' variables in a
;;     similar manner as you did for the editor variables above.
;;
;;     For example, to use "xmore", an X-compatible version of more, as your
;;     viewer, use the following settings:
;;
;;         (setq br-viewer-cmd "xmore")
;;
(defvar br-viewer-cmd nil
  "When non-nil, the OO-Browser uses a non-standard command for viewing files.
This may be either a string to invoke an external program or an Emacs
Lisp function which takes a single file argument.")

(setq br-vw1 nil br-vw2 nil br-vw3 nil br-vw4 nil br-vw5 nil
	br-vw6 nil br-vw7 nil br-vw8 nil br-vw9 nil)

;;
;;
(defvar br-skip-dir-regexps
  (if (eq system-type 'next-mach)
      ;; .E is an Eiffel system directory
      '("^CVS$" "^RCS$" "^SCCS$" "\\.lproj$" "^obj$" "\\.E$")
    '("^CVS$" "^RCS$" "^SCCS$" "\\.E$"))
  "*List of regexps matching the subdirectories that the OO-Browser will not descend when scanning source code.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(or (fboundp 'frame-width)  (defalias 'frame-width 'screen-width))
(or (fboundp 'frame-height) (defalias 'frame-width 'screen-height))
(or (fboundp 'selected-frame)
    (cond ((fboundp 'selected-screen)
	   (defalias 'selected-frame 'selected-screen))
	  (t
	    (defun selected-frame ()
	      "Always return t since current frame is always selected."
	      t))))

(defun br-after-term-init ()
  (let ((hypb (expand-file-name "hypb/" br-directory)))
    (or (featurep 'hyperbole)
	;; br-site.el should have already loaded "hversion.el" if a full
	;; Hyperbole system is available.
	(load "hyperbole" t t)
	;; Use Hyperbole mouse and keyboard handlers included with the
	;; OO-Browser since Hyperbole is not available on this system.
	(progn (or (br-member hypb load-path)
		   (setq load-path (cons hypb load-path)))
	       ;;
	       ;; Necessary to prevent action-key and assist-key from
	       ;; trying to load this Hyperbole library.
	       (provide 'hsite)
	       ;;
	       (if (boundp 'hmouse-bindings)
		   ;; Mouse support has been initialized, possibly by Hyperbole.
		   nil
		 (require 'hmouse-key)
		 ;; See the documentation for this function for instructions on how to
		 ;; setup shifted or unshifted Action and Assist mouse buttons.
		 (hmouse-shift-buttons))
	       ;;
	       (defvar hkey-init t
		 "*A non-nil value (default) at system load time binds the Action and Assist Keyboard Keys.
{M-RET} invokes the Action Key and {C-u M-RET} invokes the Assist Key.
Additionally, {C-h A} shows what the Action Key will do in the current
context (wherever point is).  {C-u C-h A} shows what the Assist Key will do.")
	       ;;
	       (and hkey-init
		    (require 'hmouse-key)
		    (not (global-key-binding "\M-\C-m"))
		    (global-set-key "\M-\C-m" 'hkey-either))
	       ;;
	       ;; Bind a key, {C-h A}, for Action Key help and {C-u C-h A}
	       ;; for Assist key help.
	       (and hkey-init
		    (not (where-is-internal 'hkey-help))
		    (define-key help-map "A" 'hkey-help))
	       
	       (defvar hkey-value nil
		 "Communicates a value between a Smart Key predicate and its actions.")

	       ;; Setup Action and Assist keys to perform only
	       ;; browser-related actions.
	       (defvar hkey-alist
		 '(
		   ((and (not (eobp))
			 (or (eolp) (and selective-display
					 (eq (following-char) ?\r)))) .
					 ((smart-scroll-up) . (smart-scroll-down)))
		   ;;
		   ;; If click in the minibuffer and reading an argument,
		   ;; accept argument or give completion help.
		   ((and (> (minibuffer-depth) 0)
			 (eq (selected-window) (minibuffer-window))
			 (not (eq hargs:reading-p 'hmenu))) .
			 ((exit-minibuffer) . (smart-completion-help)))
		   ;;
		   ;; If reading a Hyperbole menu item or a Hyperbole completion-based
		   ;; argument, allow selection of an item at point.
		   ((if (> (minibuffer-depth) 0) (setq hkey-value (hargs:at-p))) .
		    ((hargs:select-p hkey-value) .
		     (hargs:select-p hkey-value 'assist)))
		   ;;
		   ;;
		   ;; Within an OOBR-FTR buffer, an *Implementors* listing buffer, or
		   ;; an Element signatures listing buffer from the OO-Browser.
		   ((or (string-equal (buffer-name) "*Implementors*")
			(string-match "-Elements\\'" (buffer-name))
			(and (boundp 'br-feature-tags-file)
			     (stringp br-feature-tags-file)
			     (equal br-feature-tags-file buffer-file-name))) .
			     ((smart-element) . (hkey-help)))
		   ;;
		   ;; Restore window config and hide help buffer when click
		   ;; at buffer end.
		   ((if (= (point) (point-max))
			(string-match "^\\*Help\\|Help\\*$" (buffer-name))) .
		    ((hkey-help-hide) . (hkey-help-hide)))
		   ;;
		   ((and (memq major-mode '(c-mode c++-c-mode))
			 buffer-file-name (smart-c-at-tag-p)) .
			 ((smart-c) . (smart-c nil 'next-tag)))
		   ;;
		   ((if (smart-lisp-mode-p) (smart-lisp-at-tag-p)) .
		    ((smart-lisp) . (smart-lisp 'next-tag)))
		   ;;
		   ((and (eq major-mode 'java-mode) buffer-file-name
			 (or (smart-java-at-tag-p)
			     ;; Also handle Java @see cross-references.
			     (looking-at "@see[ \t]+")
			     (save-excursion
			       (and (re-search-backward "[@\n\r\f]" nil t)
				    (looking-at "@see[ \t]+"))))) .
				    ((smart-java) . (smart-java nil 'next-tag)))
		   ;;
		   ((and (eq major-mode 'c++-mode) buffer-file-name
			 ;; Don't use smart-c++-at-tag-p here since it will prevent #include
			 ;; lines from matching.
			 (smart-c-at-tag-p)) .
			 ((smart-c++) . (smart-c++ nil 'next-tag)))
		   ;;
		   ((and (eq major-mode 'objc-mode) buffer-file-name
			 (smart-objc-at-tag-p)) .
			 ((smart-objc) . (smart-objc nil 'next-tag)))
		   ;;
		   ((eq major-mode 'Buffer-menu-mode) .
		    ((smart-buffer-menu) . (smart-buffer-menu-assist)))
		   ;;
		   ((eq major-mode 'dired-mode) . 
		    ((smart-dired) . (smart-dired-assist)))
		   ;;
		   ((eq major-mode 'tar-mode) . 
		    ((smart-tar) . (smart-tar-assist)))
		   ;;
		   ((or (br-in-browser) (eq major-mode 'br-mode)) .
		    ((smart-br-dispatch) . (smart-br-assist-dispatch)))
		   ;;
		   (buffer-read-only . ((scroll-up) . (scroll-down)))
		   )
		 "Alist of predicates and form-conses for Action and Assist Keys.
When the Action or Assist Key is pressed, the first or second form,
respectively, associated with the first non-nil predicate is evaluated.")
	       ;;
	       ;; This next line must come after the definition of hkey-alist
	       ;; or the wrong definition will be used and an error will
	       ;; ensue when the {M-RET} key is pressed.
	       (require 'hui-mouse))))
  ;;
  (if (stringp br-editor-cmd)
      (let ((br-editor-cmd (downcase br-editor-cmd)))
	(and (string-match "emacs" br-editor-cmd)
	     (setq br-editor-cmd nil))))
  (if (stringp br-viewer-cmd)
      (let ((br-viewer-cmd (downcase br-viewer-cmd)))
	(and (string-match "emacs" br-viewer-cmd)
	     (setq br-viewer-cmd nil))))
  ;;
  ;; Loads menus under non-tty InfoDock, XEmacs or Emacs19; does nothing
  ;; otherwise.
  (and (not (featurep 'br-menu)) hyperb:window-system
       (or hyperb:xemacs-p hyperb:emacs19-p) (require 'br-menu))
  ;;
  (require 'br)
  (require 'hmouse-br)
  (require 'hmouse-drv)
  (br-init-autoloads))

(defun br-init-autoloads ()
  "Setup OO-Browser autoloaded functions."
;  (autoload 'br-add-class-file "br" "Add file to OO-Browser Environment" t) 
  (autoload 'br-env-browse "br-env" "Browse an existing OO-Browser Environment" t)
  (autoload 'br-env-load  "br-env" "Load a new OO-Browser Environment" t)
  (autoload 'br-to-from-viewer  "br" "Move between list and viewer windows" t)
  ;;
  (autoload 'hmail:compose      "hmail"
    "Compose mail with ADDRESS and evaluation of EXPR." t)
  (autoload 'hypb:configuration "hypb"
    "Insert Emacs configuration information into OUT-BUF or current buffer." nil)
  (autoload 'hypb:display-file-with-logo "hypb" "Display FILE with BeOpen.com logo." nil)
  ;;
  ;; Menu items could call this function before Info is loaded.
  (autoload 'Info-goto-node     "info" "Jump to specific Info node."  t)
  ;;
  (autoload 'c++-browse  "c++-browse" "C++/C OO-Browser" t)
  (autoload 'clos-browse "clos-brows" "Common Lisp OO-Browser" t)
  (autoload 'eif-browse  "eif-browse" "Eiffel OO-Browser" t)
  (autoload 'info-browse "info-brows" "Info OO-Browser" t)
  (autoload 'java-browse "java-brows" "Java OO-Browser" t)
  (autoload 'objc-browse "objc-brows" "Objective-C OO-Browser" t)
  (autoload 'python-browse "pyth-brows" "Python OO-Browser" t)
  (autoload 'smt-browse  "smt-browse" "Smalltalk OO-Browser" t)
  ;;
  ;; Hyperbole tag-related functions
  (autoload 'smart-asm-at-tag-p "hmouse-tag" "Return assembly tag name that point is within, else nil." nil nil)
  (autoload 'smart-c-at-tag-p "hmouse-tag" "Return C tag name that point is within, else nil." nil nil)
  (autoload 'smart-c++ "hmouse-tag" "Jumps to the definition of optional C++ IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching C++ tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If:
 (1) on a `#include' statement, the include file is displayed;
     Look for include file in directory lists `smart-c-cpp-include-dirs'
     and `smart-c-include-dirs'.
 (2) on a C++ identifier, the identifier definition is displayed,
     assuming the identifier is found within an `etags' generated tag file
     in the current directory or any of its ancestor directories.
 (3) if `smart-c-use-lib-man' is non-nil, the C++ identifier is
     recognized as a library symbol, and a man page is found for the
     identifier, then the man page is displayed." t nil)
  (autoload 'smart-c++-tag "hmouse-tag" nil nil nil)
  (autoload 'smart-fortran-at-tag-p "hmouse-tag" "Jumps to Fortran identifier definitions.")
  (autoload 'smart-java "hmouse-tag" "Jumps to the definition of optional Java IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching Java tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If:
 (1) within a commented @see cross-reference, the referent is displayed;
 (2) on a `package' or `import' statement, the referent is displayed;
     Look for referent files in the directory list `smart-java-package-dirs'.
 (3) on a Java identifier, the identifier definition is displayed,
     assuming the identifier is found within an `etags' generated tag file
     in the current directory or any of its ancestor directories." t nil)
  (autoload 'smart-java-tag "hmouse-tag" nil nil nil)
  (autoload 'smart-java-at-tag-p "hmouse-tag" "Return Java tag name that point is within, else nil." nil nil)
  (autoload 'smart-lisp-mode-p "hmouse-tag" "Return t if in a mode which uses Lisp symbols." nil nil)
  (autoload 'smart-objc "hmouse-tag" "Jumps to the definition of optional Objective-C IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching Objective-C tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If:
 (1) on a `#include' statement, the include file is displayed;
     Look for include file in directory lists `smart-c-cpp-include-dirs'
     and `smart-c-include-dirs'.
 (2) on an Objective-C identifier, the identifier definition is displayed,
     assuming the identifier is found within an `etags' generated tag file
     in the current directory or any of its ancestor directories.
 (3) if `smart-c-use-lib-man' is non-nil, the Objective-C identifier is
     recognized as a library symbol, and a man page is found for the
     identifier, then the man page is displayed." t nil)
  (autoload 'smart-objc-tag "hmouse-tag" nil nil nil)
  (autoload 'smart-tags-file-list "hmouse-tag" "Return list of appropriate tags files for optional CURR-DIR-OR-FILENAME or for `default-directory'.
Optional NAME-OF-TAGS-FILE is the literal filename for which to look." nil nil)
  (autoload 'smart-tags-file-path "hmouse-tag" "Expand relative FILE name by looking it up within appropriate tags files.
Return FILE unchanged if it exists relative to the current directory or
cannot be expanded via a tags file." nil nil)
  )

;;; ************************************************************************
;;; Internal functions
;;; ************************************************************************

(provide 'br-init)
