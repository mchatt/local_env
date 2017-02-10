;;; DO NOT MODIFY THIS FILE
(if (featurep 'oo-browser-autoloads) (error "Already loaded"))

;;;### (autoloads (br-complete-symbol) "br-compl" "oo-browser/br-compl.el")

(autoload 'br-complete-symbol "br-compl" "\
Complete an OO-Browser type or element or an Emacs Lisp symbol preceding point.
The symbol is compared against current Environment entries (or Emacs symbol
table entries) and any needed characters are inserted." t nil)

;;;***

;;;### (autoloads (br-env-load br-env-browse) "br-env" "oo-browser/br-env.el")

(autoload 'br-env-browse "br-env" "\
Invoke the OO-Browser on an existing or to be created Environment ENV-FILE or ENV-NAME." t nil)

(autoload 'br-env-load "br-env" "\
Load an OO-Browser Environment or specification from optional ENV-FILE, ENV-NAME or `br-env-file'.
Non-nil PROMPT means prompt user before building the Environment.
Non-nil NO-BUILD means skip the build of the Environment entirely.
Return t if the load is successful, else nil." t nil)

;;;***

;;;### (autoloads (br-names-menu) "br-name" "oo-browser/br-name.el")

(autoload 'br-names-menu "br-name" "\
Return an unnamed menu of commands that load a user's named OO-Browser Environments." nil nil)

;;;***

;;;### (autoloads (br-three-button-mouse br-two-button-mouse oo-browser) "br-start" "oo-browser/br-start.el")

(autoload 'oo-browser "br-start" "\
Prompt for an Environment over which to run the OO-Browser.
Optional prefix argument SAME-ENV-FLAG means browse the current Environment,
if any, without prompting.  Otherwise, if called interactively, give the user
a choice whether to re-browse the last Environment or to browse a new one." t nil)

(autoload 'br-two-button-mouse "br-start" "\
Sets up the Action Key within OO-Browser listing buffers for a two button mouse.
The Action Key is placed on the left mouse button." t nil)

(autoload 'br-three-button-mouse "br-start" "\
Sets up the Action Key within OO-Browser listing buffers for a three button mouse.
The Action Key is placed on the middle mouse button." t nil)

;;;***

;;;### (autoloads (br-to-from-viewer) "br" "oo-browser/br.el")

(autoload 'br-to-from-viewer "br" "\
Move point to the viewer window or back to the last recorded listing window." t nil)

;;;***

;;;### (autoloads (c++-browse) "c++-browse" "oo-browser/c++-browse.el")

(autoload 'c++-browse "c++-browse" "\
Invoke the C++ OO-Browser.
This allows browsing through C++ library and system class hierarchies.  With
an optional non-nil prefix argument ENV-FILE, prompt for Environment file to
use.  Alternatively, a string value of ENV-FILE is used as the Environment
file name.  See also the file \"br-help\"." t nil)

;;;***

;;;### (autoloads (clos-browse) "clos-brows" "oo-browser/clos-brows.el")

(autoload 'clos-browse "clos-brows" "\
Invoke the CLOS OO-Browser.
This allows browsing through CLOS library and system class hierarchies.  With
an optional non-nil prefix argument ENV-FILE, prompt for Environment file
to use.  Alternatively, a string value of ENV-FILE is used as the
Environment file name.  See also the file \"br-help\"." t nil)

;;;***

;;;### (autoloads (eif-browse) "eif-browse" "oo-browser/eif-browse.el")

(autoload 'eif-browse "eif-browse" "\
Invoke the Eiffel OO-Browser.
This allows browsing through Eiffel library and system class hierarchies.
With an optional prefix arg ENV-FILE equal to t, prompt for Environment file
to use.  Alternatively, a string value of ENV-FILE is used as the Environment
file name.  See also the file \"br-help\"." t nil)

;;;***

;;;### (autoloads (info-browse) "info-brows" "oo-browser/info-brows.el")

(autoload 'info-browse "info-brows" "\
Invoke the Info OO-Browser.
This allows browsing through Info library and system class hierarchies.  With
an optional non-nil prefix argument ENV-FILE, prompt for Environment file to
use.  Alternatively, a string value of ENV-FILE is used as the Environment
file name.  See also the file \"br-help\"." t nil)

;;;***

;;;### (autoloads (java-browse) "java-brows" "oo-browser/java-brows.el")

(autoload 'java-browse "java-brows" "\
Invoke the Java OO-Browser.
This allows browsing through Java library and system class hierarchies.  With
an optional non-nil prefix argument ENV-FILE, prompt for Environment file to
use.  Alternatively, a string value of ENV-FILE is used as the Environment
file name.  See also the file \"br-help\"." t nil)

;;;***

;;;### (autoloads (objc-browse) "objc-brows" "oo-browser/objc-brows.el")

(autoload 'objc-browse "objc-brows" "\
Invoke the Objective-C OO-Browser.
This allows browsing through Objective-C library and system class
hierarchies.  With an optional non-nil prefix argument ENV-FILE, prompt for
Environment file to use.  Alternatively, a string value of ENV-FILE is used
as the Environment file name.  See also the file \"br-help\"." t nil)

;;;***

;;;### (autoloads (python-browse) "pyth-brows" "oo-browser/pyth-brows.el")

(autoload 'python-browse "pyth-brows" "\
Invoke the Python OO-Browser.
This allows browsing through Python library and system class hierarchies.
With an optional non-nil prefix argument ENV-FILE, prompt for Environment
file to use.  Alternatively, a string value of ENV-FILE is used as the
Environment file name.  See also the file \"br-help\"." t nil)

;;;***

;;;### (autoloads (smt-browse) "smt-browse" "oo-browser/smt-browse.el")

(autoload 'smt-browse "smt-browse" "\
Invoke the Smalltalk OO-Browser.
This allows browsing through Smalltalk library and system class hierarchies.
With an optional non-nil prefix argument ENV-FILE, prompt for Environment
file to use.  Alternatively, a string value of ENV-FILE is used as the
Environment file name.  See also the file \"br-help\"." t nil)

;;;***

;;;### (autoloads nil "_pkg" "oobr/_pkg.el")

(package-provide 'oo-browser :version 4.5 :type 'regular)

;;;***

(provide 'oo-browser-autoloads)
