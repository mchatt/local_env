;;!emacs
;;
;; FILE:         br-tree.el
;; SUMMARY:      Interface between textual and graphical OO-Browsers.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     mouse, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:    12-Oct-90
;; LAST-MOD:      9-Jun-99 at 18:05:26 by Bob Weiner
;;
;; Copyright (C) 1990-1995, 1997, 1998  BeOpen.com
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;;
;;   Requires the X Window system Version 11, Windows or NEXTSTEP.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'br-lib)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar *br-tree-prog-name*
  (let* ((prog
	  (cond ((or (eq window-system 'x)
		     (null window-system))
		 "xoobr")
		;; Windows
		((memq window-system '(mswindows win32 w32 pm)) "oobr.exe")
		;; NeXTSTEP
		(t "TreeView")))
	 (gui-oo-browser (expand-file-name prog br-directory)))
    ;; Look for program in br-directory, exec-directory and
    ;; then in user's $PATH.
    (if (or (file-executable-p gui-oo-browser)
	    (progn (setq gui-oo-browser (expand-file-name prog exec-directory))
		   (file-executable-p gui-oo-browser)))
	gui-oo-browser
      prog))
  "Program to run for hierarchical display of classes.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun br-tree (&optional arg)
  "Start the appropriate tree application with descendency tree of current class.
With optional prefix ARG, include a descendency tree for each class in
the current listing buffer." 
  (interactive "P")
  (let* ((classes (if arg
		    (br-this-level-classes)
		  (br-find-class-name-as-list)))
	 (ch (delq nil (mapcar (function (lambda (c) (br-get-children c)))
			      classes))))
    (if (or ch br-show-features)
	(br-tree-load classes)
      (beep)
      (message "No descendants to display."))))

(defun br-tree-graph ()
  "Start the appropriate tree application with the tree from the current listing buffer."
  (interactive)
  (let* ((tree) (indent) (entry) (min-indent 8000) (min-count 0)
	 (feature-match (format "^%s " br-feature-type-regexp)))
    (save-excursion
      (goto-char (point-max))
      (while (and (= (forward-line -1) 0)
		  (looking-at "\\([ \t]*\\)\\(.+\\)"))
	(setq indent (br-buffer-substring (match-beginning 1) (match-end 1))
	      entry (length indent)
	      min-indent (cond ((= entry min-indent)
				(setq min-count (1+ min-count))
				entry)
			       ((< entry min-indent)
				(setq min-count 1)
				entry)
			       (min-indent))
	      entry (br-buffer-substring (match-beginning 2) (match-end 2))
	      entry (if (string-match feature-match entry)
			(concat (char-to-string (aref entry 0))
				(substring entry 2)
				"^^" (prin1-to-string (br-feature-get-tag)))
		      entry)
	      tree (cons (concat indent entry "\n") tree))))
    (or (= min-count 1)
	(setq tree (cons (concat *br-tree-root-name* "\n")
			 (mapcar (function
				  (lambda (node) (concat "  " node))) tree))))
    (br-tree-load tree t)))

(defun br-tree-do-cmd (lang env cmd node)
  ;; Load necessary Environment
  (if (not (equal env br-env-file))
      (let ((br (intern-soft
		  (concat lang "browse"))))
	(if (br-in-browser) (funcall br env) (funcall br env t))))
  ;; Do command
  (let ((hpath:display-where
	 (if (or (not (boundp 'hpath:display-where))
		 (eq hpath:display-where 'other-window))
	     'this-window ;; Force display in selected window.
	   hpath:display-where)))
    (cond ((br-feature-tag-p node)
	   (br-feature (string-equal cmd "br-view") node))
	  ;;
	  ;; node = class name
	  ((string-equal cmd "br-view")
	   (br-view nil nil node))
	  ((string-equal cmd "br-edit")
	   (br-view nil t node))
	  (t (beep)
	     (message
	      (format "(OO-Browser):  Illegal command: %s" cmd))))))

(defun br-tree-features-toggle ()
  "Toggle between showing and hiding features when `br-tree' is invoked to display descendants graphically."
  (interactive)
  (setq br-show-features (not br-show-features))
  (message "New graphical OO-Browsers will %sshow features."
	   (if br-show-features "" "not ")))

(defun br-tree-kill ()
  "Kill all current graphical OO-Browser sub-processes."
  (interactive)
  (if (br-kill-process-group br-tree-name br-tree-num
			     "Graphical OO-Browsers")
      (setq br-tree-num 0)))

(defun br-tree-load (classes-or-tree &optional tree-p)
  "Start the appropriate tree application using trees from CLASSES-OR-TREE.
Optional TREE-P non-nil means CLASSES-OR-TREE is a tree ready for display."
  (interactive (list "sClass to show descendency graph of: "))
  (if (and br-env-file (not br-env-spec))
      (let ((obuf (current-buffer))
	    (tree-file (expand-file-name
			(format "%s%d.obr"
				(user-real-login-name)
				(setq br-tree-num (1+ br-tree-num)))
			(br-temp-directory))))
	(if classes-or-tree
	    (progn (find-file tree-file)
		   (widen)
		   (setq buffer-read-only nil)
		   (erase-buffer)
		   ;; Start file with Envir file name
		   (insert "^^" br-lang-prefix "^^" br-env-file "\n")
		   (if tree-p
		       (mapcar 'insert classes-or-tree)
		     (br-tree-build classes-or-tree))
		   (untabify 1 (point-max))
		   (save-buffer)
		   (kill-buffer (current-buffer))
		   (switch-to-buffer obuf)
		   (if (memq window-system '(x mswindows win32 w32 pm))
		       (br-tree-x-load-tree-file tree-file)
		     (br-tree-nx-load-tree-file tree-file)))))))

(defun br-tree-nx-load-tree-file (tree-file)
  "Load a pre-written TREE-FILE and display it in an X OO-Browser."
  (setq delete-exited-processes t)
  (let ((proc (get-process br-tree-name)))
    (if (and proc (eq (process-status proc) 'run))  ;; existing tree browser
	;; Send it an open file command.
	(call-process "open" nil 0 nil "-a"
		      (file-name-nondirectory *br-tree-prog-name*)
		      tree-file)
      (let ((default-directory (file-name-as-directory
				 (expand-file-name "tree-nx" br-directory))))
	(setq proc (start-process
		     br-tree-name nil *br-tree-prog-name*
		     tree-file))
	(if proc
	    (progn (set-process-filter proc 'br-tree-filter)
		   (process-kill-without-query proc)
		   ))))))

(defun br-tree-x-load-tree-file (tree-file)
  "Load a pre-written TREE-FILE and display it in an X OO-Browser."
  (setq delete-exited-processes t)
  (let ((proc)
	(windowed-process-io t))
    (setq proc (start-process 
		(concat br-tree-name (int-to-string br-tree-num))
		nil
		*br-tree-prog-name*
		tree-file))
    (if proc
	(progn (set-process-filter proc 'br-tree-filter)
	       (process-kill-without-query proc)))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defconst *br-tree-root-name* "NO-ROOT" 
  "Name to give root tree node when graph with no root is used as input.")

(defun br-tree-build (class-list &optional indent offset)
  "Insert descendant trees starting with classes from CLASS-LIST.
Indent each class in CLASS-LIST by optional INDENT spaces (default is 0 in
order to ensure proper initialization).  Offset each child level by optional
OFFSET spaces from its parent (which must be greater than zero, default 2)."
  (or indent (setq indent 0))
  (or offset (setq offset 2))
  (let ((prev-expansion-str " ...")
	ch expand-subtree)
    (if (= indent 0)
	(progn (setq br-tmp-class-set nil)
	       (if (= (length class-list) 1)
		   nil
		 (insert *br-tree-root-name* "\n")
		 (setq indent offset))))
    (if class-list
	(progn 
	  (indent-to indent)
	  (mapcar (function (lambda (c)
		     (setq expand-subtree (br-set-cons br-tmp-class-set c)
			   ch (if expand-subtree (br-get-children c)))
		     (indent-to indent)
		     (insert c)
		     (and (not expand-subtree)
			  (br-has-children-p c)
			  (insert prev-expansion-str))
		     (insert "\n")
		     (if (and br-show-features
			      (br-tree-build-features
			       c expand-subtree (+ indent offset) offset))
			 nil
		       (if ch
			   (br-tree-build ch (+ indent offset) offset)))))
		  class-list))))
  (if (= indent 0) (setq br-tmp-class-set nil)))

(defun br-tree-build-features (c expand-subtree indent offset)
  "Each language under which this function is called must define its own
version of `br-feature-signature-to-name'."
  (let ((feature-list) (ch))
    (and expand-subtree
	 (setq feature-list
	       (mapcar
		(function
		 (lambda (feature-tag)
		   (concat (br-feature-signature-to-name feature-tag nil t)
			   "^^" (prin1-to-string feature-tag))))
		(br-list-features c)))
	 (progn
	   (mapcar
	    (function
	     (lambda (feature)
	       (indent-to indent)
	       (insert feature "\n")))
	    feature-list)
	   (if (setq ch (if expand-subtree (br-get-children c)))
	       (br-tree-build ch indent offset))
	   t))))

(defun br-tree-filter (process output-str)
  (let ((lang-prefix)
	(env-name)
	(cmd-name)
	(node)
	(feature-tag))
    (if (not (string-match "\n" output-str))
	(setq br-cmd-str (concat br-cmd-str output-str))
      (setq br-cmd-str (concat br-cmd-str
			       (substring output-str 0 (match-beginning 0))))
      (if (and (> (length br-cmd-str) 9)
	       (equal (substring br-cmd-str -4)
		      " ..."))
	  (setq br-cmd-str (substring br-cmd-str 0 -4)))
      ;; Is a command only if starts with ^^
      (if (and (> (length br-cmd-str) 1)
	       (equal (substring br-cmd-str 0 2) "^^")
	       (string-match
		"^\\^\\^\\(.+\\)\\^\\^\\(.+\\)\\^\\^\\(.+\\)\\^\\^\\(.+\\)"
		br-cmd-str))
	  (progn
	    (setq lang-prefix (substring br-cmd-str
					 (+ (match-beginning 1) 2)
					 (match-end 1))
		  env-name (substring br-cmd-str
				      (match-beginning 2)
				      (match-end 2))
		  cmd-name (substring br-cmd-str
				      (match-beginning 3)
				      (match-end 3))
		  node (substring br-cmd-str
				  (match-beginning 4)
				  (match-end 4))
		  br-cmd-str nil)
	    ;;
	    ;; `node' is either a class name or a feature-tag that we
	    ;; must convert from string format.
	    (setq feature-tag (car (read-from-string node)))
	    (if (br-feature-tag-p feature-tag)
		(setq node feature-tag))
	    (br-tree-do-cmd lang-prefix env-name
			    cmd-name node))
	(beep)
	(message "`%s': invalid command from the graphical OO-Browser"
		 br-cmd-str)
	(setq br-cmd-str nil)))))


;;; ************************************************************************
;;; Private functions
;;; ************************************************************************


(defvar br-cmd-str nil
  "Command string sent from graphical OO-Browser to the textual OO-Browser.")

(defvar br-show-features nil
  "Non-nil means add features as child nodes in each graphical descendancy view.
Defaults to nil.")

(defvar br-tree-num 0)
(defvar br-tree-name "OO-Browser")

(provide 'br-tree)
