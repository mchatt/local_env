;;!emacs
;;
;; FILE:         hmouse-br.el
;; SUMMARY:      Hyperbole Key control for the OO-Browser.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     mouse, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:    Sep-04-90
;; LAST-MOD:     10-May-01 at 13:32:59 by Bob Weiner
;;
;; Copyright (C) 1990-1995, 1997, 1998  BeOpen.com
;; See the file BR-COPY for license information.
;;
;; This file is part of the OO-Browser.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'br)

;;; ************************************************************************
;;; smart-br functions
;;; ************************************************************************

;;; Unused unless the "br.el" library, part of the OO-Browser package, has
;;; been loaded.

(defun smart-br ()
  "Controls OO-Browser listing buffers with one key or mouse key.

Invoked via a key press when in an OO-Browser listing window.  It assumes
that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If key is pressed:
 (1) in a blank buffer or at the end of a buffer, browser help
     information is displayed in the viewer window;
 (2) on a default class name, the statically defined instances of the default
     class are listed;
 (3) at the beginning of a (non-single character) class name, the class'
     ancestors are listed;
 (4) at the end of an entry line, the listing is scrolled up;
 (5) on the `...', following a class name, point is moved to the class
     descendency expansion;
 (6) before an element entry, the element's implementors are listed;
 (7) anywhere else on an entry line, the entry's source is displayed for
     editing."

  (interactive)
  (br-browse)
  (cond ((eobp)
	 (br-help)
	 (and action-mouse-key-prev-window
	      (select-window action-mouse-key-prev-window)))
	((eolp) (smart-scroll-up))
	((br-at-default-class-p) (br-features 1))
	((br-at-feature-p)
	 (if (save-excursion
		(skip-chars-backward " \t")
		(bolp))
	     (br-implementors)
	   (br-feature)
	 (if (br-in-browser) (br-to-from-viewer))))
	((and (save-excursion
		(skip-chars-backward " \t")
		(bolp))
	      (let ((cl (br-find-class-name)))
		(and cl (/= (length cl) 1))))
	 (br-ancestors))
	((br-to-tree))
	((br-edit)
	 (if (br-in-browser) (br-to-from-viewer))
	 t)))

(defun smart-br-assist ()
  "Controls OO-Browser listing buffers with one assist-key or mouse assist-key.

Invoked via an assist-key press when in an OO-Browser listing window.  It
assumes that its caller has already checked that the assist-key was pressed in
an appropriate buffer and has moved the cursor to the selected buffer.

If assist-key is pressed:
 (1) in a blank buffer, a selection list of buffer files is displayed;
 (2) on a default class name, the statically defined instances of
     the default class are listed;
 (3) at the beginning of a (non-single character) class, the class'
     descendants are listed;
 (4) at the end of an entry line, the listing is scrolled down;
 (5) on the `...', following a class name, point is moved to the class
     expansion;
 (6) anywhere else on a class line, the class' elements are listed;
 (7) anywhere else on an element line, the element's implementors
     are listed;
 (8) on a blank line following all entries, the current listing buffer
     is exited."
  
  (interactive)
  (br-browse)
  (cond ((equal 0 (string-match br-buffer-prefix-blank (buffer-name)))
	 (br-buffer-menu))
	((eobp) (br-exit-level 1))
	((eolp) (smart-scroll-down))
	((br-at-default-class-p) (br-features 1))
	((br-at-feature-p) (br-implementors))
	((and (save-excursion
		(skip-chars-backward " \t")
		(bolp))
	      (let ((cl (br-find-class-name)))
		(and cl (/= (length cl) 1))))
	 (br-descendants))
	((br-to-tree))
	(t (br-features 1))))

(defun smart-br-dispatch ()
  (cond ((or (br-listing-window-p) (eq major-mode 'br-mode))
	 ;; In an OO-Browser listing window.
	 (smart-br))
	((eq major-mode 'Info-mode) (smart-info))
	((eolp) (smart-scroll-up))
	((and (boundp 'br-src-file-regexp)
	      buffer-file-name
	      (fboundp (symbol-function 'br-to-definition))
	      (string-match br-src-file-regexp buffer-file-name))
	 (br-to-definition))
	((and action-mouse-key-prev-window
	      (or (smart-br-cmd-select nil)
		  (error "(Action Key): No command bound to key."))))
	(t (scroll-up))))

(defun smart-br-assist-dispatch ()
  (if (or (br-listing-window-p) (eq major-mode 'br-mode))
      ;; In an OO-Browser listing window.
      (smart-br-assist)
    (cond ((eq major-mode 'Info-mode)
	   (smart-info-assist))
	  ((eolp) (smart-scroll-down))
	  ((and action-mouse-key-prev-window
		(or (smart-br-cmd-select 'assist)
		    (error "(Assist Key): No command bound to key."))))
	  (t (scroll-down)))))

(defun smart-br-cmd-select (&optional assist-flag)
  "Selects an OO-Browser command with its key binding at point.
By default executes the command, with optional ASSIST-FLAG non-nil, shows help for
command.  Returns t if a command is selected.  Nil indicates no key binding was
found on the current line.  Key bindings are delimited by {}."
  (let ((start) (end) (tmp-buf) (tmp-buf-nm) (obuf (current-buffer)))
    (and (save-excursion
	   (or (eobp) (forward-char))
	   (save-excursion
	     (beginning-of-line)
	     (setq start (point)))
	   (and (re-search-backward "\\(^\\|[^\\]\\){" start t)
		(progn 
		  (goto-char (match-end 0))
		  (setq start (point))
		  (save-excursion
		    (end-of-line)
		    (setq end (point)))
		  (and (re-search-forward "[^\\]}" end t)
		       (setq end (1- (point)))))))
	 (progn
	   (setq tmp-buf-nm "*smart-br-tmp*"
		 tmp-buf (progn (if (get-buffer tmp-buf-nm)
				    (kill-buffer tmp-buf-nm))
				(get-buffer-create tmp-buf-nm)))
	   (or tmp-buf
	       (error
		"(Action Key): (smart-br-cmd-select) - Can't create tmp-buf."))
	   (copy-to-buffer tmp-buf start end)
	   (set-buffer tmp-buf)
	   (let ((case-fold-search nil) (case-replace t)
		 (keys)
		 (pref-arg action-mouse-key-prefix-arg))
	     ;; Quote Control and Meta key names
	     (goto-char (point-min))
	     (replace-regexp "[ \t]+" "")
	     (goto-char (point-min))
	     (replace-string "SPC" "\040")
	     (goto-char (point-min))
	     (replace-string "DEL" "\177")
	     (goto-char (point-min))
	     (replace-regexp "ESC" "M-")
	     (goto-char (point-min))
	     ;; Unqote special {} chars.
	     (replace-regexp "\\\\\\([{}]\\)" "\\1")
	     (goto-char (point-min))
	     (if (looking-at "C-u")
		 (progn (delete-char 3)
			(and (or (null pref-arg)
				 (equal pref-arg 1))
			     (setq pref-arg '(4)))))
	     (while (search-forward "C-" nil t)
	       (replace-match "")
	       (setq keys (1+ (- (downcase (following-char)) ?a)))
	       (delete-char 1)
	       (insert keys))
	     (goto-char (point-min))
	     (while (search-forward "M-" nil t)
	       (replace-match "")
	       (setq keys (+ 128 (downcase (following-char))))
	       (delete-char 1)
	       (insert keys))
	     (setq keys (buffer-string))
	     (kill-buffer tmp-buf-nm)
	     (set-buffer obuf)
	     (and (boundp 'action-mouse-key-prev-window)
		  action-mouse-key-prev-window
		  (select-window action-mouse-key-prev-window))
	     (let ((current-prefix-arg pref-arg)
		   (binding (key-binding keys)))
	       (if binding
		   (progn
		     (if assist-flag
			 (br-cmd-help keys)
		       (call-interactively binding))
		     t))))))))

(defun smart-element ()
  "Jumps to the source line associated with an interim OOBR-FTR feature tags file entry.
Assumes caller has checked that point is within an OOBR-FTR buffer, an
*Implementors* listing buffer, or an Element signatures listing buffer from
the OO-Browser.  If on a tag entry line, jumps to the source line for the
tag.  If on a class name line, jumps to the class definition.  If on a
pathname line or line preceding it, jumps to the associated file."
  (save-excursion
    (beginning-of-line)
    (cond
     ;; On a file separator or file entry line, display the file.
     ((save-excursion
	(and (or (and (eq (following-char) ?\^L)
		      (zerop (forward-line 1)))
		 (and (zerop (forward-line -1))
		      (eq (following-char) ?\^L)
		      (zerop (forward-line 1))))
	     (looking-at "\"?\\([^,\n\r\"]+\\)\"?$")))
      (let ((file (buffer-substring (match-beginning 1) (match-end 1))))
	(if (and (fboundp 'ibut:label-set) (fboundp 'hproperty:but-flash))
	    (progn (ibut:label-set file (match-beginning 1) (match-end 1))
		   (hproperty:but-flash)))
	(hpath:find file)))
     ;;
     ;; On an OO-Browser V3 element tag line, display its source code definition.
     ((save-excursion (beginning-of-line) (looking-at br-tag-fields-regexp))
      (let* ((bol (progn (back-to-indentation) (point)))
	     (eol (progn (end-of-line) (point)))
	     (ftr-tag (buffer-substring bol eol))
	     (def-file))
	(setq def-file (br-feature-v3-def-file (regexp-quote ftr-tag)))
	(if def-file
	    (progn (if (and (fboundp 'ibut:label-set)
			    (fboundp 'hproperty:but-flash))
		       (progn (ibut:label-set ftr-tag bol eol)
			      (hproperty:but-flash)))
		   (if (br-edit-feature-from-tag ftr-tag def-file)
		       nil
		     (error "(Action Key): (smart-element) - `%s' def not found in \"%s\""
			    (br-feature-signature-to-name ftr-tag) def-file)))
	  (error "(Action Key): (smart-element) - No implementor definitions for `%s'"
		 (br-feature-signature-to-name ftr-tag)))))
     ;;
     ;; On an OO-Browser V4 element tag line, display its source code definition.
     ((save-excursion (beginning-of-line) (looking-at "\\s-*\\["))
      (let* ((eol (progn (end-of-line) (point)))
	     (bol (progn (back-to-indentation) (point)))
	     (ftr-str (br-buffer-substring eol bol))
	     (ftr-tag (read (current-buffer)))
	     (def-file (br-feature-tag-path ftr-tag)))
	(if def-file
	    (progn (if (and (fboundp 'ibut:label-set)
			    (fboundp 'hproperty:but-flash)) 
		       (progn (ibut:label-set ftr-str bol eol)
			      (hproperty:but-flash)))
		   (if (br-edit-feature-from-tag ftr-tag def-file)
		       nil
		     (error "(Action Key): (smart-element) - `%s' def not found in \"%s\""
			    (br-feature-tag-name ftr-tag) def-file)))
	  (error "(Action Key): (smart-element) - No implementor definitions for `%s'"
		 (br-feature-tag-name ftr-tag)))))
     ;;
     ;; On a OO-Browser V4 feature signature line, indented below its class
     ;; name line (e.g. Implementors buffer)
     ((save-excursion (back-to-indentation) (looking-at ".*[ \t|,\{\;]"))
      (let* ((ftr-tag (br-feature-get-tag))
	     (bol (progn (back-to-indentation) (point)))
	     (eol (progn (end-of-line) (point)))
	     (ftr-sig (if ftr-tag (br-feature-tag-signature ftr-tag)
			(buffer-substring bol eol)))
	     (def-file (if ftr-tag (br-feature-tag-path ftr-tag)))
	     (ftr-name (if ftr-tag (br-feature-tag-name ftr-tag))))
	(if def-file
	    (progn (if (and (fboundp 'ibut:label-set)
			    (fboundp 'hproperty:but-flash))
		       (progn (ibut:label-set ftr-sig bol eol)
			      (hproperty:but-flash)))
		   (if (br-edit-feature-from-tag ftr-tag def-file)
		       nil
		     (error
		      "(Action Key): (smart-element) - `%s' def not found in \"%s\""
		      ftr-name def-file)))
	  (if ftr-name
	      (error
	       "(Action Key): (smart-element) - No implementor definitions for `%s'"
	       ftr-name)
	    (error
	     "(Action Key): (smart-element) - Select a class or feature line")))))
     ;;
     ;; Assume this is a class name entry, display the class def.
     (t (let* ((bol (progn (back-to-indentation) (point)))
	       (eol (progn (end-of-line) (point)))
	       (class (br-buffer-substring bol eol)))
	  (if (and (fboundp 'ibut:label-set) (fboundp 'hproperty:but-flash))
	      (progn (ibut:label-set class bol eol) (hproperty:but-flash)))
	  (br-edit nil class))))))

;;; ************************************************************************
;;; Hyperbole info browsing functions
;;; ************************************************************************

(autoload 'Info-handle-in-note "hmous-info"
          "Follows Info documentation references.")
(autoload 'smart-info "hmous-info" "Follows Info documentation references." t)
(autoload 'smart-info-assist "hmous-info"
          "Follows Info documentation references." t)

(provide 'hmouse-br)
