;;!emacs
;;
;; FILE:         br-c-ft.el
;; SUMMARY:      OO-Browser C construct handling.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     c, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:     3-May-95 at 16:47:05
;; LAST-MOD:     10-May-01 at 05:45:05 by Bob Weiner
;;
;; Copyright (C) 1995, 1996, 1997, 1999  BeOpen.com
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar   c-default-classes
  '("[constant]" "[enumeration]" "[enum_label]" "[function]" "[macro]"
    "[structure]" "[type]" "[union]" "[variable]")
  "*List of default class names of C constructs handled by the OO-Browser.

If you add a class to this list, you also need to add appropriate code to
handle the class in \"ootags.c\".")

(defconst c++-function-identifier
  (concat "[_~\<a-zA-Z][^\]\[ \t\n\r\f:\;.,~{}()]*")
  "Regular expression matching a C++ or G++ function name.")

(defconst c++-operator-name-regexp
  (format "operator ?\\(%s%s%s%s%s\\)"
	  (mapconcat
	   'regexp-quote
	   ;; Each item in this list which contains a prefix for another item
	   ;; must precede the other item or the shorter item will be matched
	   ;; improperly, for example, "==" must precede "=".
	   '("~" "||" "|=" "|" "^=" "^" "[]" "?" ">>=" ">>" ">=" ">" "==" "="
	     "<=" "<<=" "<<" "<" "/=" "/" "->*" "->" "-=" "--" "-" "+=" "++"
	     "+" "*=" "*" "()" "&=" "&&" "&" "%=" "%" "!=" "!")
	   "\\|")
	  ;; Handles delete, new, delete[] and new[].
	  "\\|delete ?\\[?\\]?\\|new *\\[?\\]?\\|"
	  ;; Handles operator <type>, <type>* and <type>& operators.
	  c++-function-identifier " ?[*&]*"
	  ;; The next expression matches to entries such as `int operator'
	  ;; where ootags has stripped the type of operator.  We still want to
	  ;; register this as a function even though its definition won't be
	  ;; found.  This typically happens if a space occurs between the
	  ;; operator keyword and an `=' or `*' operator character.
	  "\\|$")
  "Regular expression matching explicitly named C++ or G++ operators within OO-Browser feature tag files.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun c-add-default-classes ()
  (br-add-default-classes c-default-classes))

(defun c-build-element-tags ()
  "Create C constructs tags file for the current Environment.
Call this after building the language-specific feature tags file."
  (if (not (and br-c-tags-flag
		(br-member br-lang-prefix
			   '("c++-" "eif-" "java-" "objc-" "python-"))
		(stringp br-tags-tmp-file)))
      nil
    (cond ((and hyperb:microcruft-os-p (or (not br-shell-executable)
					   (not br-ootags-executable)))
	   (if (not br-shell-executable)
	       (progn
		 (message "No sh/csh/bash found, skipping C construct indexing")
		 (sit-for 2))))
	  ((not br-ootags-executable)
	   (message
	    "No ootags found, skipping C construct indexing")
	   (sit-for 2))
	  (t
	   ;; If C tags have already been added to feature tags, then the
	   ;; feature tags buffer ends with ^L.
	   (br-feature-set-tags-buffer)
	   ;; Remove any old C construct tags.
	   (if (progn (goto-char (point-max))
		      (skip-chars-backward "\n")
		      (beginning-of-line)
		      (eq (following-char) ?\^L))
	       (if (and br-env-version (string-lessp br-env-version "02.11.02"))
		   nil
		 (let ((c-tags-start (condition-case ()
					 (read (current-buffer))
				       (error nil))))
		   (if c-tags-start
		       (delete-region c-tags-start (point-max)))
		   (c-build-element-tags-internal)))
	     (c-build-element-tags-internal))))))

(defun c-build-element-tags-internal ()
  ;; Build new C construct tags.
  (message "Building C construct index...")
  (if hyperb:microcruft-os-p
      (apply 'call-process br-shell-executable
	     nil nil nil
	     (expand-file-name "br-c-tags" br-directory)
	     br-ootags-executable br-tags-tmp-file
	     (mapcar 'expand-file-name
		     (delq nil (append br-sys-search-dirs
				       br-lib-search-dirs))))
    (apply 'call-process (expand-file-name "br-c-tags" br-directory)
	   nil nil nil br-ootags-executable br-tags-tmp-file
	   (mapcar 'expand-file-name
		   (delq nil (append br-sys-search-dirs br-lib-search-dirs)))))
  (goto-char (point-max))
  (let ((c-tags-start (point)))
    (if (file-readable-p br-tags-tmp-file)
	(insert-file-contents br-tags-tmp-file))
    (goto-char (point-max))
    ;; Insert delimiter to mark both the start and end of C tags insertion.
    (insert "\^L " (int-to-string c-tags-start) "\n")
    (if (and (file-readable-p br-tags-tmp-file)
	     (file-writable-p br-tags-tmp-file))
	(delete-file br-tags-tmp-file))
    ;;
    ;; Fix up C++ operator tags which ootags doesn't format properly.
    (if (equal br-lang-prefix "c++-")
	(let* ((entry-prefix-regexp
		(format "^[^%s \n\r/\\]+%s[^%s\n\r]*%s"
			c++-type-tag-separator c++-type-tag-separator
			c++-type-tag-separator c++-type-tag-separator))
	       (op-regexp (format "%s\\(.*\\(%s\\).*\\)"
				  entry-prefix-regexp
				  c++-operator-name-regexp))
	       (op-tag-replacement (format "[function]%s- \\2%s\\1"
					   c++-type-tag-separator
					   c++-type-tag-separator))
	       ;; This is presently the same as op-tag-replacement but may
	       ;; be changed in the future.
	       (op-unknown-tag-replacement
		(format "[function]%s- \\2%s\\1"
			c++-type-tag-separator c++-type-tag-separator)))
	  (goto-char c-tags-start)
	  ;; Remove any scoped entries which the OO-Browser
	  ;; handles without ootags help.
	  (delete-matching-lines (concat entry-prefix-regexp "[^\(\n\r]+::"))
	  ;; Fix up global operator entries.
	  (while (re-search-forward op-regexp nil t)
	    (if (= (match-beginning 3) (match-end 3))
		;; Type of operator was dropped by ootags.
		(replace-match op-unknown-tag-replacement t)
	      (replace-match op-tag-replacement t)))))
    (goto-char c-tags-start)
    ;; Remove tag files which have no entries.
    (while (re-search-forward "^\^L\n.*\n\^L\n" nil t)
      (replace-match "\^L\n")
      (forward-line -1))
    ;; Remove // style comments
    (goto-char c-tags-start)
    (while (re-search-forward "^\\(\\[.*\\)[ \t]*//.*" nil t)
      (replace-match "\\1"))
    ;; Normalize C tag entries.
    (goto-char c-tags-start)
    (while (re-search-forward "[ \t\r][ \t\r]+" nil t)
      (replace-match " ")))
  (message "Building C construct index...Done"))

(defun c-within-comment-p ()
  "Return non-nil if point is within a multi-line C comment."
  ;; Generally don't have to check whether patterns are matched on single line
  ;; comments  ( // ...) since the regexps to match to will preclude this.
  ;; Ignore comments of the form //***, which look like C comments when
  ;; searching backward but are actually single line comments.
  (save-excursion
    (and (re-search-backward "\\(^\\|[^/]\\)/\\*\\|\\*/" nil t)
	 (not (looking-at "\\*/")))))

(provide 'br-c-ft)
