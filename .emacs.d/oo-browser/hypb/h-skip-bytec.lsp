;;!emacs
;;
;; FILE:         h-skip-bytec.lsp
;; SUMMARY:      Functions that should not be byte-compiled.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     mouse, hypermedia
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:     8-Oct-92 at 17:17:10
;; LAST-MOD:     13-Jun-99 at 01:06:58 by Bob Weiner
;;
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; Copyright (C) 1992-1995, BeOpen.com and the Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of Hyperbole.
;;
;; DESCRIPTION:  
;;
;;   DON'T byte-compile this file or its functions may not work.
;;   If we knew why they won't work, they wouldn't be in here.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;; For some reason, using this in byte-compiled form causes first character
;;; after mouse key depress to be dropped from input queue when running
;;; Emacs under X.  The non-byte-compiled form works fine.

(defun hmouse-set-point (args)
  "Sets point to Smart Key press/release location given by ARGS.
Returns argument list including x and y frame coordinates in characters and
lines."
  (and (car args) (listp (car args)) (setq args (car args)))
  (if (not hyperb:window-system)
      (point-marker)
    (let ((point-args (hmouse-set-point-at args)))
      (cond (hyperb:xemacs-p
	     (if (eventp current-mouse-event)
		 (copy-event current-mouse-event)))
	    ((equal hyperb:window-system "next")
	     (let ((win (car args)))
	       (list win
		     (+ (nth 1 args) (nth 0 (window-edges win)))
		     (+ (nth 2 args) (nth 1 (window-edges win))))))
	    ((equal hyperb:window-system "apollo") point-args)
	    (t args)))))

(provide 'h-skip-bytec)
