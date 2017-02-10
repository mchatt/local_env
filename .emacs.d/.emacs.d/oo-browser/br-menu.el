;;!emacs
;;
;; FILE:         br-menu.el
;; SUMMARY:      Pulldown and popup menus for the OO-Browser.
;; USAGE:        GNU Emacs Lisp Library
;; KEYWORDS:     mouse, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          BeOpen.com
;;
;; ORIG-DATE:    27-Oct-93 at 21:13:36
;; LAST-MOD:     10-May-01 at 04:53:25 by Bob Weiner
;;
;; DESCRIPTION:  
;;
;;   Load this file to add a menubar entry for invoking the OO-Browser under
;;   XEmacs and GNU Emacs.  InfoDock automatically adds such an entry under
;;   its Software menu.
;;
;; DESCRIP-END.
;;
;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defconst br-menu-common-body
  (delq
   nil
   (list
    '("Class"
      ["Concept-Manual"      (id-info "(oo-browser)Top-Level Classes") t]
      ["Menu-Manual"         (id-info "(oo-browser)Class Menu") t]
      "----"
      ["Edit-Definition"     br-edit-entry                  t]
      ["View-Definition"     br-view-entry                  t]
      "----"
      ["Edit-Named"          (br-edit-entry t)
       :active t :keys "C-u e"]
      ["View-Named"          (br-view-entry t)
       :active t :keys "C-u v"]
      "----"
      ["Match-from-Listing"  br-match                       t]
      ["Where-is-Named?"     (br-where t)
       :active t :keys "C-u w"]
      ["Where-is-Entry?"     br-where                       t]
      "----"
      ["Ancestors"           br-ancestors                   t]
      ["Attributes"          br-attributes                  t]
      ["Children"            br-children                    t]
      ["Descendants"         br-descendants                 t]
      ["Features"            br-features                    t]
      ["Implementors"        br-implementors                t]
      ["Info"                br-entry-info                  t]
      ["Level"               br-at                          t]
      ["Parents"             br-parents                     t]
      ["Protocols"           br-protocols                   t]
      ["Routines"            br-routines                    t]
      "----"
      ["Class-Statistics"    br-class-stats                 t]
      )
    (list
     "Environment"
     '["Concept-Manual"      (id-info "(oo-browser)Environments") t]
     '["Menu-Manual"         (id-info "(oo-browser)Environment Menu") t]
     "----"
     '["Create-or-Load"      (id-tool-invoke 'br-env-browse)
       :active t :keys "C-c C-l"]
     '["Display-Env-List"    br-names-display (not (br-names-empty-p))]
     '["Rebuild"             br-env-rebuild   br-env-file]
     '["Statistics"          br-env-stats     br-env-file]
     "----"
     '["Add-Name"            br-name-add      t]
     '["Change-Name"         br-name-change   (not (br-names-empty-p))]
     '["Remove-Name"         br-name-remove   (not (br-names-empty-p))]
     '["Replace-Env-of-Name" br-name-replace  (not (br-names-empty-p))]
     "----"
     '["Delete-Class"        br-delete        br-env-file]
     "----"
     '["Save"                br-env-save      br-env-file]
     )
    '("Feature"
      ["Concept-Manual"      (id-info "(oo-browser)Browsing Elements") t]
      ["Menu-Manual"         (id-info "(oo-browser)Feature Menu") t]
      "----"
      ["Edit-Definition"     br-edit-entry                  t]
      ["View-Definition"     br-view-entry                  t]
      "----"
      ["Edit-Declaration"    br-feature-edit-declaration    t]
      ["View-Declaration"    br-feature-view-declaration    t]
      "----"
      ["View-Friend-Def"     br-view-friend                 t]
      "----"
      ["Edit-Named"          (br-edit-entry t)
       :active t :keys "C-u e"]
      ["View-Named"          (br-view-entry t)
       :active t :keys "C-u v"]
      "----"
      ["Current-Attributes"  br-attributes                  t]
      ["Current-Features"    br-features                    t]
      ["Current-Routines"    br-routines                    t]
      "----"
      ["All-Attributes"      (br-attributes 2)
       :active t :keys "C-u ="]
      ["All-Features"        (br-features 2)
       :active t :keys "C-u f"]
      ["All-Routines"        (br-routines 2)
       :active t :keys "C-u r"]
      "----"
      ["Implementors"        br-implementors                t]
      ["Signature"           br-feature-signature           t]
      )
    '("Graphical"
      ["Concept-Manual"      (id-info "(oo-browser)Browsing Graphically") t]
      ["Menu-Manual"         (id-info "(oo-browser)Graphical Menu") t]
      "----"
      ["Class-Descendants-View"   br-tree                   t]
      ["Listing-Descendants-View" (br-tree t)
       :active t :keys "C-u M-d"]
      ["Listing-Graphical-View"   br-tree-graph             t]
      "----"
      ["Kill-Graphical-Views"     br-tree-kill              t]
      )
    '("List-Window"
      ["Concept-Manual"      (id-info "(oo-browser)Usage") t]
      ["Menu-Manual"         (id-info "(oo-browser)List-Window Menu") t]
      "----"
      ["Write (Save as)"     br-write-buffer                t]
      "----"
      ["Count-Entries"       br-count                       t]
      ["Order-Entries"       (progn (br-order 1) (br-unique))
       :active t :keys "o u"]
      "----"
      ["All-Ancestors"       (br-ancestors 2)
       :active t :keys "C-u a"]
      ["All-Attributes"      (br-attributes 2)
       :active t :keys "C-u ="]
      ["All-Children"        (br-children t)
       :active t :keys "C-u c"]
      ["All-Descendants"     (br-descendants t)
       :active t :keys "C-u d"]
      ["All-Features"        (br-features 2)
       :active t :keys "C-u f"]
      ["All-Implementors"    (br-implementors t)
       :active t :keys "C-u I"]
      ["All-Levels"          (br-at t)
       :active t :keys "C-u @"]
      ["All-Parents"         (br-parents t)
       :active t :keys "C-u p"]
      ["All-Protocols"       (br-protocols t)
       :active t :keys "C-u P"]
      ["All-Routines"        (br-routines 2)
       :active t :keys "C-u r"]
      "----"
      ["Show-All-Classes"      br-show-all-classes          t]
      ["Show-All-Lib-Classes"  (br-lib-top-classes t)
       :active t :keys "C-u l"]
      ["Show-All-Sys-Classes"  (br-sys-top-classes t)
       :active t :keys "C-u s"]
      ["Show-Top-Classes"      br-show-top-classes          t]
      ["Show-Top-Lib-Classes"  br-lib-top-classes           t]
      ["Show-Top-Sys-Classes"  br-sys-top-classes           t]
      "----"
      ["Narrow-by-10"          br-resize-narrow             t]
      ["Widen-by-10"           br-resize-widen              t]
      "----"
      ["Exit-this-Listing"     br-exit-level                t]
      )
    (if (fboundp 'infodock-options-menu)
	(infodock-options-menu)
      '("Options"
	["Concept-Manual"      (id-info "(oo-browser)Options") t]
	["Menu-Manual"         (id-info "(oo-browser)Options Menu") t]
	"----"
	["Keep-Viewed-Classes" br-toggle-keep-viewed
	 :style toggle :selected br-keep-viewed-classes
	 :active t :keys "M-0 v"]
	["Graphical-Descendant-Features" br-tree-features-toggle
	 :style toggle :selected br-show-features]
	["List-Protocols-with-Classes" (br-protocols 0)
	 :style toggle :selected (if (br-protocol-support-p)
				     br-protocols-with-classes-flag)
	 :active (br-protocol-support-p) :keys "M-0 P"]
	["Show-Inherited-Features"
	 (setq br-inherited-features-flag
	       (not br-inherited-features-flag))
	 :style toggle :selected br-inherited-features-flag
	 :active t :keys "M-0 f"]
	["Use-Vi-as-Editor"
	 (if br-editor-cmd
	     (br-setup-internal)
	   (br-setup-external))
	 :style toggle :selected br-editor-cmd :active t]
	["3-Button-Mouse"
	 (if (= hyperb:mouse-buttons 3)
	     (br-two-button-mouse) (br-three-button-mouse))
	 :style toggle :selected (= hyperb:mouse-buttons 3) :active t]))
    '("View-Window"
      ["Concept-Manual"      (id-info "(oo-browser)Viewing and Editing") t]
      ["Menu-Manual"         (id-info "(oo-browser)View-Window Menu") t]
      "----"
      ["Select-Code-Buffer"  br-buffer-menu                 t]
      "----"
      ["Full-Frame"          br-view-full-frame             t]
      ["Kill-Buffer"         br-kill                        t]
      ["Move-To-or-From"     br-to-from-viewer              t]
      "----"
      ["Scroll-Backward"     br-viewer-scroll-down          t]
      ["Scroll-Forward"      br-viewer-scroll-up            t]
      "----"
      ["Scroll-Backward-One-Line"   br-viewer-scroll-down-by-line   t]
      ["Scroll-Forward-One-Line"    br-viewer-scroll-up-by-line     t]
      "----"
      ["To-Buffer-Beginning"        br-viewer-beginning-of-buffer  t]
      ["To-Buffer-End"              br-viewer-end-of-buffer        t]
      )))
  "The middle menu entries common to all OO-Browser menus.")

(defconst br-menu-common-preamble
  (delq
   nil
   (list
    "OO-Browser"
    '["About"               (hypb:display-file-with-logo
			     (expand-file-name "BR-FEATURES" br-directory))
      t]
    '["Language-Manual"     br-info-language-specific      t]
    '["Program-Manual"      (id-info "(oo-browser)Top") t]
    '["Menu-Manual"         (id-info "(oo-browser)OO-Browser Menu") t]
    '["What-is-New?"        (hypb:display-file-with-logo
			     (expand-file-name "BR-RELEASE" br-directory)) t]
    "----"
    (if (or (featurep 'infodock) (featurep 'xemacs))
	'("Load-Env-by-Name" :filter br-names-menu)
      '("Load-Env-by-Name"))
    "----"
    '["Copyright"           br-copyright                   t]
    '["Help-Commands"       br-help                        t]
    '["Help-Mode"           describe-mode                  t]
    '["Help-Mouse"          br-help-ms                     t]
    "----"
    '["Discuss-via-Email"
      (progn (br-quit)
	     (mail nil "oo-browser-discuss@lists.sourceforge.net"
		   "Replace this line with a descriptive sentence.\nComments: Discuss a topic on the oo-browser list.")
	     (goto-char (point-min))
	     (search-forward "Subject: " nil t)) t]
    '["Get-Support-via-Email"
      (progn (br-quit)
	     (mail nil "support@deepware.com"
		   "Replace this line with a descriptive sentence.\nComments: Use a pre-paid support credit with Deepware.")
	     (goto-char (point-min))
	     (search-forward "Subject: " nil t)) t]
    )))

(defconst br-menu-common-postamble
  '("----"
    ["Reinitialize"        br-refresh                   t]
    ["Exit-this-Listing"   br-exit-level                t]
    "----"
    ["Exit-Temporarily"    (id-tool-quit '(br-quit))
     :active t :keys "C-u q"]
    ["Quit"                (id-tool-quit '(br-quit t))
     :active t :keys "q"]
    ))

(if (featurep 'infodock)
(defconst br-menu-external
  (delq
   nil
   (list
    "%_OO-Browser"
    '["About"             (hypb:display-file-with-logo
			   (expand-file-name "BR-FEATURES" br-directory))
      t]
    '["Manual"            (id-tool-invoke id-man-oo-browser)  t]
    '["What-is-New?"      (hypb:display-file-with-logo
			   (expand-file-name "BR-RELEASE" br-directory)) t]
    "----"
    '["Create-or-Load-Env"
      (let ((id-tool-visible-flag 'visible))
	(id-tool 'br-env-browse 'OO-Browser 'br-mode 1)) t]
    '["Display-Env-List"  br-names-display
      (and (fboundp 'br-names-empty-p) (not (br-names-empty-p)))]
    (if (or (featurep 'infodock) (featurep 'xemacs))
	'("Load-Env-by-Name" :filter br-names-menu)
      '("Load-Env-by-Name"))
    '["Rebuild-Env"       br-env-rebuild 
      (and (fboundp 'br-env-rebuild) br-env-file)]
    "----"
    '["Continue"          (let ((id-tool-visible-flag 'visible)
				(current-prefix-arg 1))
			    (id-tool-invoke id-tool-oo-browser))
      (and (boundp 'br-lang-prefix) (stringp br-lang-prefix)
	   (boundp '*br-save-wconfig*) *br-save-wconfig* t)]
    '["Invoke"            (let ((id-tool-visible-flag 'visible))
			    (id-tool-invoke id-tool-oo-browser)) t]
    "----"
    '["Add-Env-Name"           br-name-add      (fboundp 'br-name-add)]
    '["Change-Env-Name"        br-name-change
      (and (fboundp 'br-names-empty-p) (not (br-names-empty-p)))]
    '["Remove-Env-Name"        br-name-remove
      (and (fboundp 'br-names-empty-p) (not (br-names-empty-p)))]
    '["Replace-Env-of-Name"    br-name-replace
      (and (fboundp 'br-names-empty-p) (not (br-names-empty-p)))]
    "----"
    '["Env-Statistics"    br-env-stats
      (and (fboundp 'br-env-stats) br-env-file)]
    "----"
    '["Delete-Env-Class"  br-delete
      (and (fboundp 'br-delete) br-env-file)]
    "----"
    (if (< hyperb:mouse-buttons 3)
	'["3-Button-Mouse"    br-three-button-mouse t])
    "----"
    '["Reinitialize"      br-refresh
      (and (fboundp 'br-in-browser) (br-in-browser))]
    '["Exit-Temporarily"  (id-tool-quit '(br-quit))
      (and (fboundp 'br-in-browser) (br-in-browser))]
    '["Quit"              (id-tool-quit '(br-quit t))
      (and (fboundp 'br-in-browser) (br-in-browser))]
    ))
  "OO-Browser invocation and management menu used outside of the browser user interface.
This version is used under InfoDock.")

;; else
(defconst br-menu-external
  (delq
   nil
   (list
    "OO-Browser"
    '["About"               (hypb:display-file-with-logo
			     (expand-file-name "BR-FEATURES" br-directory))
      t]
    '["Manual"             (progn (require 'info)
				  ;; Force execution of Info-mode-hook which
				  ;; adds the OO-Browser man directory to
				  ;; Info-directory-list.
				  (info "oo-browser")) t]
    '["What-is-New?"       (hypb:display-file-with-logo
			    (expand-file-name "BR-RELEASE" br-directory)) t]
    "----"
    '["Create-or-Load-Env" br-env-browse t]
    '["Display-Env-List"  br-names-display
      (and (fboundp 'br-names-empty-p) (not (br-names-empty-p)))]
    (if (or (featurep 'infodock) (featurep 'xemacs))
	'("Load-Env-by-Name" :filter br-names-menu)
      '("Load-Env-by-Name"))
    '["Rebuild-Env"       br-env-rebuild
      (and (fboundp 'br-env-rebuild) br-env-file)]
    "----"
    '["Continue"         (oo-browser t)
      (and (boundp 'br-lang-prefix) (stringp br-lang-prefix)
	   (boundp '*br-save-wconfig*) *br-save-wconfig* t)]
    '["Invoke"           oo-browser t]
    "----"
    '["Add-Env-Name"           br-name-add      (fboundp 'br-name-add)]
    '["Change-Env-Name"        br-name-change
      (and (fboundp 'br-names-empty-p) (not (br-names-empty-p)))]
    '["Remove-Env-Name"        br-name-remove
      (and (fboundp 'br-names-empty-p) (not (br-names-empty-p)))]
    '["Replace-Env-of-Name"    br-name-replace
      (and (fboundp 'br-names-empty-p) (not (br-names-empty-p)))]
    "----"
    '["Env-Statistics"    br-env-stats
      (and (fboundp 'br-env-stats) br-env-file)]
    "----"
    '["Delete-Env-Class"  br-delete
      (and (fboundp 'br-delete) br-env-file)]
    "----"
    (if (< hyperb:mouse-buttons 3)
	'["3-Button-Mouse"    br-three-button-mouse t])
    "----"
    '["Reinitialize"      br-refresh
      (and (fboundp 'br-in-browser) (br-in-browser))]
    '["Exit-Temporarily"  (br-quit)
      (and (fboundp 'br-in-browser) (br-in-browser))]
    '["Quit"              (br-quit t)
      (and (fboundp 'br-in-browser) (br-in-browser))]
    ))
  "OO-Browser invocation and management menu used outside of the browser user interface.
This version is used under XEmacs and GNU Emacs.")
)


;;; This definition is used by InfoDock only.
(defconst id-menubar-br
  (cons (append br-menu-common-preamble br-menu-common-postamble)
	br-menu-common-body))

;;; This definition is used by InfoDock, XEmacs and GNU Emacs.
(defconst id-popup-br-menu
   (append br-menu-common-preamble
	   `("----" ,@ br-menu-common-body)
	   br-menu-common-postamble))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;; The menubar definition is used only by XEmacs and Emacs19.
(defun br-menu-external-setup (&optional menu-path)
  ;; When the InfoDock default menubar is active, InfoDock automatically 
  ;; adds the external OO-Browser menu below its Software menu.
  ;; Otherwise, the external OO-Browser menu should be added to the
  ;; menubar.
  (if (and (featurep 'infodock)
	   (car (find-menu-item current-menubar '("Software")))
	   (fboundp 'add-submenu))
      ;; Remove possible occurrence of `br-menu-external' to force a reload 
      ;; using the current menu definition.
      (progn (delete-menu-item '("Software" "OO-Browser"))
	     ;; Add menu used to invoke the OO-Browser.
	     (add-submenu '("Software") br-menu-external "Tags"))
    ;; Remove possible occurrence of `br-menu-external' to force a reload 
    ;; using the current menu definition.
    (delete-menu-item '("OO-Browser"))
    ;; Add menu used to invoke the OO-Browser.
    (if (fboundp 'add-submenu)
	(add-submenu menu-path br-menu-external)
      ;; For GNU Emacs only.
      (add-hook 'menu-bar-update-hook 'br-menu-update-env-list)
      (add-menu menu-path (car br-menu-external) (cdr br-menu-external)))
    ;; The next line forces a menubar refresh in some versions of XEmacs
    ;; which have an event handler bug that prevents display of the
    ;; OO-Browser menu on the menubar until the next user event occurs.
    (sit-for 0.001)))

;;; The menubar definition is used only by XEmacs and GNU Emacs
;;; or under InfoDock with XEmacs menus active.
(defun br-menubar-menu-setup (&optional menu-path)
  "Add an OO-Browser menu to the menubar for each listing buffer."
  (if (and (boundp 'current-menubar)
	   (or hyperb:emacs19-p current-menubar))
      (progn
	(set-buffer-menubar (copy-sequence current-menubar))
	;; Remove possible occurrence of `br-menu-external'.
	(delete-menu-item '("OO-Browser"))
	;; Add OO-Browser menu used while the browser user interface is active.
	(if (fboundp 'add-submenu)
	    (add-submenu menu-path id-popup-br-menu)
	  ;; For GNU Emacs only.
	  (add-hook 'menu-bar-update-hook 'br-menu-update-env-list)
	  (add-menu menu-path (car id-popup-br-menu) (cdr id-popup-br-menu)))
	;; The next line forces a menubar refresh in some versions of XEmacs
	;; which have an event handler bug that prevents display of the
	;; OO-Browser menu on the menubar until the next user event occurs.
	(sit-for 0.001))))

;;; This definition is used only by XEmacs and Emacs19.
(defun br-popup-menu (event)
  "Popup the OO-Browser listing buffer menu."
  (interactive "@e")
  (mouse-set-point event)
  (cond
   ;; GNU Emacs
   ((fboundp 'popup-menu-internal)
    (popup-menu-internal id-popup-br-menu '*id-popup-br-menu*))
   ;; XEmacs
   (t (popup-menu id-popup-br-menu))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun br-menu-update-env-list ()
  "Used under GNU Emacs only to update the dynamic Environment name list."
  (add-menu '("OO-Browser") "Load-Env-by-Name" (br-names-menu nil)))

(cond ((null hyperb:window-system))
      ((featurep 'infodock)
       ;; InfoDock under a window system
       (require 'id-menubars)
       (id-menubar-set 'br-mode 'id-menubar-br))
      (hyperb:xemacs-p
       ;; XEmacs under a window system
       (add-hook 'br-mode-hook 'br-menubar-menu-setup))
      (hyperb:emacs19-p
       ;; Emacs 19 under a window system
       (require 'lmenu)
       (add-hook 'br-mode-hook 'br-menubar-menu-setup)))

;; Initialize menu used to invoke the OO-Browser.
(if (and hyperb:window-system
	 (not (featurep 'infodock)))
    (progn
      ;; Initialize now for when this is loaded after startup.
      (and (boundp 'current-menubar) 
	   (or hyperb:emacs19-p current-menubar)
	   (br-menu-external-setup))
      ;; Initialize at startup.  This really is needed.
      (add-hook 'after-init-hook 'br-menu-external-setup)))

(provide 'br-menu)
