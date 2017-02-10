;;!emacs
;;
;; FILE:         br-eif-ft.el
;; SUMMARY:      Eiffel OO-Browser class and feature functions.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:    03-Oct-90
;; LAST-MOD:     10-May-01 at 05:42:39 by Bob Weiner
;;
;; Copyright (C) 1990-1996  BeOpen.com
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'br-c-ft)
(require 'eif-calls)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defconst eif-type-tag-separator ","
  "String that separates a tags type from its normalized definition form.
This should be a single character which is unchanged when quoted for use as a
literal in a regular expression.")

(defconst eif-tag-fields-regexp
  ;; The \\\\? below is necessary because we sometimes use this expression to
  ;; test against a string that has been regexp-quoted and some of the
  ;; characters in br-feature-type-regexp will then be preceded by \\.
  (format "^\\([^%s \n]+\\)%s\\\\?\\(%s \\)\\([^%s\n]+\\)"
	  eif-type-tag-separator eif-type-tag-separator
	  br-feature-type-regexp eif-type-tag-separator)
 "Regexp matching the fields of an Eiffel feature tag line.
Group 1 is the class of the feature.  Group 2 is the prefix preceding the
feature when displayed within a listing buffer.  Group 3 is the feature
name.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun eif-add-default-classes ()
  (if br-c-tags-flag (c-add-default-classes)))

(defun eif-feature-implementors (ftr-name)
  "Return unsorted list of Eiffel feature tags which implement FTR-NAME."
  (eif-feature-matches (concat "^" (regexp-quote ftr-name) "$")))

(defun eif-feature-name-to-regexp (name)
  "Converts feature NAME into a regular expression matching the feature's name tag."
  (if (string-match (concat "^" br-feature-type-regexp " ") name)
      (setq name (substring name (match-end 0))))
  (format "%s%s%s %s[ \n\r]"
	  eif-identifier eif-type-tag-separator br-feature-type-regexp
	  (regexp-quote name)))

(defun eif-feature-signature-to-name (feature-sig-or-tag &optional with-class for-display)
  "Extract the feature name without its class name from FEATURE-SIG-OR-TAG.
If optional WITH-CLASS is non-nil, class name and :: are prepended to the
name returned.  If optional FOR-DISPLAY is non-nil, a feature type character
is prepended to the name for display in a browser listing."
  (cond ((br-feature-tag-p feature-sig-or-tag)
	 (br-feature-tag-name feature-sig-or-tag with-class for-display))
	((string-match (concat eif-type-tag-separator
			       "\\(" br-feature-type-regexp " \\)")
		       feature-sig-or-tag)
	 (let ((class (substring feature-sig-or-tag 0 (match-beginning 0)))
	       (name (substring feature-sig-or-tag (match-end 0))))
	   (cond ((and with-class for-display)
		  (concat class "::" (substring feature-sig-or-tag
						(match-beginning 1))))
		 (with-class
		  (concat class "::" name))
		 (for-display
		  (substring feature-sig-or-tag (match-beginning 1)))
		 (t name))))
	(t feature-sig-or-tag)))

(defun eif-feature-signature-to-regexp (signature)
  "Given an Eiffel class or feature SIGNATURE, return regexp to match its definition."
  (let ((regexp) name type)
    (cond ((string-match (concat "\\`" br-feature-type-regexp " ")
			 signature)
	   (setq name (substring signature (match-end 0))
		 type (string-to-char
		       (substring signature 0 1)))
	   (setq regexp
		 (cond ((memq type '(?- ?1 ?> ?/))
			;; routine
			(eif-routine-to-regexp name))
		       ((eq type ?=)
			;; attribute
			(eif-attribute-to-regexp name)))))
	  ((equal 0 (string-match eif-identifier signature))
	   ;; Assume is a class name
	   (setq regexp
		 (concat eif-class-name-before
			 (regexp-quote signature)
			 eif-class-name-after))))
    (or regexp
	(error "(eif-feature-signature-to-regexp): Invalid format, `%s'"
	       signature))))

(defun eif-output-feature-tags (feature-file feature-tags-list)
  "Write Eiffel FEATURE-FILE's FEATURE-TAGS-LIST into `br-feature-tags-file'.
Assume `br-feature-tags-init' has been called."
  (interactive)
  (save-excursion
    (br-feature-set-tags-buffer)
    (goto-char 1)
    ;; Delete any prior feature tags associated with feature-file
    (if (search-forward feature-file nil 'end)
	(progn (forward-line -1)
	       (let ((start (point)))
		 (search-forward "\^L" nil 'end 2)
		 (backward-char 1)
		 (delete-region start (point)))))
    (if feature-tags-list
	(progn (insert "\^L\n")
	       ;; Quote pathname to avoid read errors on MS OSes.
	       (prin1 feature-file (current-buffer))
	       (insert "\n")
	       (mapcar (function (lambda (tag) (insert tag "\n")))
		       feature-tags-list)))))

(defun eif-scan-features-in-class (class start end)
  "Return unordered list of Eiffel feature definitions in CLASS.
START and END give buffer region to search."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (let ((attributes-and-routines (eif-parse-features t)))
	(append
	 (mapcar
	  (function (lambda (routine)
		      (concat class eif-type-tag-separator routine)))
	  (cdr attributes-and-routines))
	 (mapcar
	  (function (lambda (attribute)
		      (concat class eif-type-tag-separator attribute)))
	  (car attributes-and-routines)))))))

(defun eif-to-definition (&optional identifier)
  "If point is within an Eiffel class or feature name, try to move to its definition.
With optional IDENTIFIER, do the same instead for it."
  (interactive)
  (let ((cl (or identifier (eif-find-class-name))))
    (cond
     ((eif-keyword-p) nil)
     ((br-check-for-class cl))
     ((eif-feature cl))
     ((progn
	(beep)
	(message
	 "(OO-Browser):  Select an Eiffel identifier to move to its definition.")
	nil))
     )))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun eif-export-feature-p ()
  "Return nil unless point is within a class export clause."
  (save-excursion
    (let ((end (point)))
      (beginning-of-line)
      ;; If in a comment, return nil.
      (if (search-forward "--" end t)
	  nil
	(goto-char (point-min))
	(and (re-search-forward eif-export-key-regexp end t)
	     (not (re-search-forward "^\\(inherit\\|feature\\)\\([ \t]\\|$\\)" end t)))))))

(defun eif-feature (&optional ftr)
  "Return nil if definition is not found for optional FTR or feature declared at point."
  (interactive)
  (let ((class-deferred)
	(class)
	(deferred-p)
	(ftr-def-class))
    (cond ((or ftr (and (eif-export-feature-p)
			(setq ftr (eif-to-feature-decl))))
	   (if (and (setq class-deferred (eif-get-class-name-from-source))
		    (setq class (car class-deferred)
			  deferred-p (cdr class-deferred)
			  ftr-def-class (eif-find-ancestors-feature
					 (list class) deferred-p ftr)))
	       (cond ((equal (car ftr-def-class) class) t)
		     ((equal (cdr ftr-def-class) ftr)
		      ;; Feature inherited but not renamed.
		      (message
		       "Feature `%s' of class `%s' inherited from class `%s'."
		       ftr class (car ftr-def-class)))
		     ;; Feature inherited and renamed.
		     (t (message "Feature `%s', class `%s' from feature `%s', class `%s'."
				 ftr class (cdr ftr-def-class)
				 (car ftr-def-class))
			t))
	     (beep)
	     (message "(OO-Browser):  `%s' feature not found." ftr)
	     t))
	  ((and (not ftr) (eif-feature-def-p)))
	  ;;
	  ;; Later we might add the case of a feature invocation here.
	  ;;
	  )))

(defun eif-feature-def-p ()
  "If point is within a feature definition's name, display feature including leading comments."
  (let ((opoint (point)))
    (beginning-of-line)
    (if (or (looking-at eif-routine-regexp)
	    (looking-at eif-attribute-regexp))
	(progn (setq opoint (match-beginning eif-feature-name-grpn))
	       (br-display-code opoint))
      (goto-char opoint)
      nil)))

(defun eif-feature-map-tags (function regexp)
  "Apply FUNCTION to all current feature tags that match REGEXP and return a list of the results."
  (let ((identifier-chars (concat "[" eif-identifier-chars "]*")))
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

(defun eif-feature-matches (regexp)
  "Return an unsorted list of feature tags whose names match in part or whole to REGEXP.
^ and $ characters may be used to match to the beginning and end of a feature name,
respectively."
  (eif-feature-map-tags 'identity regexp))

(defun eif-find-ancestors-feature (class-list deferred-class ftr)
  (let* ((classes class-list)
	 (cl)
	 (file)
	 (found-ftr))
    (if (null class-list)
	nil
      (while (and (not found-ftr) classes)
	(setq cl (car classes)
	      file (br-class-path cl))
	(and file (setq found-ftr
			(br-feature-found-p file ftr deferred-class)))
	;; If found-ftr is a cons cell, then only one parent class need
	;; be searched to look for ftr.
	(if (consp found-ftr)
	    (setq class-list (list (car found-ftr))
		  ftr (cdr found-ftr)))
	(setq classes (cdr classes)))
      (cond ((consp found-ftr)
	     (eif-find-ancestors-feature class-list deferred-class ftr))
	    ((null found-ftr)
	     (eif-find-ancestors-feature 
	      (apply 'append (mapcar (function
				       (lambda (cl) (br-get-parents cl)))
				     class-list))
	      deferred-class
	      ftr))
	    (t (cons cl ftr))))))

(defun eif-find-class-name ()
  "Return class name that point is within, else nil."
  (if (= (point) (point-max)) (skip-chars-backward " \t\n\r"))
  (save-excursion
    (skip-chars-forward " \t")
    (skip-chars-backward eif-identifier-chars)
    (skip-chars-backward " \t\n\r\f")
    (backward-char 1)
    (and (looking-at eif-class-name-pat)
	 (br-buffer-substring (match-beginning 2)
			      (match-end 2)))))

(defun eif-find-feature (feature-name)
  "With point selecting a class in a listing buffer, move point to definition of FEATURE-NAME in viewer window.
Move point and return non-nil iff FEATURE-NAME is found."
  (interactive "sFeature to find: ")
  ;; If selected class is displayed, don't go to start of class
  (if (equal (br-class-path (br-find-class-name))
	     (progn
	       (br-to-from-viewer)
	       (expand-file-name buffer-file-name)))
      nil
    (br-edit))
  (if (eiffel-find-feature feature-name)
      (progn (recenter 0)
	     t)
    (br-to-from-viewer)
    (and (interactive-p)
	 (progn
	   (beep)
	   (message "(OO-Browser):  No `%s' feature found." feature-name)))))

(defun eif-feature-locate-p (feature-tag)
  (let (start class feature-sig)
    (if (br-feature-tag-p feature-tag)
	(setq class (br-feature-tag-class feature-tag)
	      name (br-feature-tag-name feature-tag nil nil)
	      feature-sig (br-feature-tag-signature feature-tag))
      (setq feature-sig feature-tag
	    name (br-feature-name feature-tag)
	    class nil))
    ;;
    ;; First move to the proper class implementation, so that if two
    ;; classes in the same file have the same feature signature, we still
    ;; end up at the right one.
    (cond (class
	   (if (not (br-default-class-p class))
	       (re-search-forward
		(concat eif-class-name-before (regexp-quote class)
			eif-class-name-after)
		nil t)))
	  ((string-match (concat "\\`[^\]\[]+" eif-type-tag-separator)
			 feature-sig)
	   (setq class (substring feature-sig 0 (1- (match-end 0))))
	   (re-search-forward
	    (concat eif-class-name-before (regexp-quote class)
		    eif-class-name-after)
	    nil t)))
    (if (not (re-search-forward
	      (eif-feature-signature-to-regexp feature-sig) nil t))
	nil
      (goto-char (match-beginning 0))
      (if (search-forward name nil t) (goto-char (match-beginning 0)))
      (setq start (point))
      (br-display-code start))))

(defun eif-keyword-p ()
  "Return t if point is within an Eiffel keyword, else nil."
  (if (= (point) (point-max)) (skip-chars-backward " \t\n\r"))
  (save-excursion
    (skip-chars-forward " \t")
    (skip-chars-backward eif-identifier-chars)
    (and (looking-at eif-identifier)
	 (hash-key-p (br-buffer-substring (match-beginning 0)
					  (match-end 0))
		     eif-reserved-words-htable))))

(defun eif-locate-feature (ftr ftr-pat)
  (let ((opoint (point)))
    (goto-char (point-min))
    (if (and (re-search-forward "^feature\\([ \t]\\|$\\)" nil t)
	     (re-search-forward ftr-pat nil t))
	(progn (goto-char (match-beginning 0))
	       (if (search-forward ftr nil t)
		   (goto-char (match-beginning 0)))
	       (setq opoint (point))
	       (br-display-code opoint))
      (goto-char opoint)
      (and (interactive-p) (error "Feature `%s' not found." ftr)))))

(defun eif-renamed-feature-p (ftr)
  (goto-char (point-min))
  (let ((rename-regexp "[ \t\n\r]+rename[ \t\n\r]")
	(rename-match
	 (concat eif-identifier "[ \t\n\r]+as[ \t\n\r]+" ftr "[,; \t\n\r]"))
	(prev-feature-nm)
	(prev-class)
	(parents))
    (while (and (setq prev-feature-nm
		      (and (re-search-forward rename-regexp nil t)
			   (re-search-forward rename-match nil t)))
		(setq prev-feature-nm
		      (br-buffer-substring (match-beginning 1) (match-end 1))
		      prev-class (match-beginning 0))
		(progn (backward-char 1)
		       (eif-in-comment-p))))
    (if prev-feature-nm
	(progn (goto-char prev-class)
	       (setq parents (eif-get-parents-from-source
			      buffer-file-name nil))
	       (if (re-search-backward (concat
					"[^[][ \t\n\r]+\\("
					(mapconcat 'identity parents "\\|")
					"\\)")
				       nil t)
		   (progn (setq prev-class (br-buffer-substring
					    (match-beginning 1)
					    (match-end 1)))
			  (cons prev-class prev-feature-nm))
		 (beep)
		 (message
		  "(OO-Browser):  Internal error - no class associated with rename clause."))))))

(defun eif-to-feature-decl ()
  (let ((end))
    (while (and (progn (skip-chars-backward "^, \t\n\r")
		       (and (not (eq (preceding-char) ?,))
			    (not (looking-at "export[ \t\n\r]+"))))
		(progn (skip-chars-backward " \t\n\r")
		       (setq end (point))
		       (beginning-of-line)
		       (if (search-forward "--" end t)
			   (progn (goto-char end)
				  (skip-chars-forward " \t\n\r")
				  nil)
			 (goto-char end)
			 t)))))
  (if (looking-at "export[ \t\n\r]+")
      (goto-char (match-end 0))
    (skip-chars-forward " \t\n\r"))
  (if (looking-at eif-feature-name)
      (br-buffer-substring (match-beginning 0) (match-end 0))))

;; Prefixed with `eiffel' rather than `eif' since works as a standalone
;; feature in buffers whose major mode is `eiffel-mode'.  It is used by the
;; browser but may also be used standalone.
;;
(defun eiffel-find-feature (feature-name)
  "Move point to start of feature named FEATURE-NAME in current buffer.
Display feature including all preceding comments at the top of the window.
Move point and return non-nil iff FEATURE-NAME is found."
  (interactive "sFeature to find: ")
  (cond ((eif-locate-feature
	  feature-name (eif-routine-to-regexp feature-name)))
	((eif-to-attribute feature-name)
	 (br-display-code (point))
	 (back-to-indentation)
	 t)))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defconst eif-feature-name
  (concat 
   "\\("
   "\\(prefix[ \t]+\"\\(not\\|\\+\\|-\\)\"\\)"
   "\\|infix[ \t]+\"\\(div\\|mod\\|^\\|<=?\\|>=?\\|\+\\|-\\|\\*\\|/"
                   "\\|and then\\|and\\|or else\\|or\\|xor\\|implies\\)"
   "\\|" eif-identifier "\\)")
  "Regexp matching any Eiffel feature name.
Will also match class names and keywords, so tests for these should precede
use of this expression.")

(defconst eif-export-key-regexp
  "\\(^[ \t]*\\|[ \t]+\\)export[ \t\n\r]+"
  "Regexp matching the Eiffel export keyword in context.")

(defconst eif-class-repeat (concat "repeat[ \t]+" eif-identifier)
  "Match to an Eiffel `repeat <class>' phrase.  Grouping 1 is class name.")

(defconst eif-exported-feature
  (concat "\\(,\\|export[ \t\n\r]+\\(--.*[ \t\n\r]+\\)*\\)"
	  eif-feature-name "\\([ \t]*{[^\}]+}\\)?"
	  "\\([ \t]*[\n\r,]\\|[ \t]+--\\)")
  "Regexp to match to a feature declaration in an export clause.
  Exclude `repeat <class>' phrases.  Feature name is grouping 3.")


(provide 'br-eif-ft)
