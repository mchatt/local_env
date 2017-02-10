;; Environnement
(set-language-environment "UTF-8")
(setq inhibit-startup-message t)

;; Install package
(require 'package) ;; You might already have this line
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; Affichage
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(column-number-mode t)
(setq truncate-partial-width-windows nil)
(setq ring-bell-function 'ignore)
(display-time-mode t)

;; Raccourcis
(global-set-key (kbd "C-c h") 'replace-string)
(global-set-key (kbd "C-c j") 'replace-regexp)
(global-set-key (kbd "C-c o") 'bury-buffer)
(global-set-key (kbd "C-c k") 'kill-this-buffer)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c g") 'gdb)

;; Line number left
(require 'linum)
(global-linum-mode t)
(setq linum-format "%4d \u2502 ")

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'monokai t)

;; Indentation : no tab + 4 spaces + remove whitespace at end of line
(setq c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(autoload 'nuke-trailing-whitespace "whitespace" nil t)

;; syntaxic highlighting + maximum colors
(global-font-lock-mode t)
(setq font-lock-maximum-size nil)

;; hightlight current line
(global-hl-line-mode 1)

;; change backup directory
(setq backup-directory-alist `(("." . "~/.saves")))

;; code formatter
(load "~/.emacs.d/tools/clang-format.el")

;; sr-speedbar
(load "~/.emacs.d/tools/sr-speedbar.el")
(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)
;(sr-speedbar-open)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(send-mail-function nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Puppet syntax highlighting
(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))


;; (require 'tabbar)
;; (tabbar-mode)

;; (defun my-tabbar-buffer-groups () ;; customize to show all normal files in one group
;;    "Returns the name of the tab group names the current buffer belongs to.
;;  There are two groups: Emacs buffers (those whose name starts with '*', plus
;;  dired buffers), and the rest.  This works at least with Emacs v24.2 using
;;  tabbar.el v1.7."
;;    (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
;;                ((eq major-mode 'dired-mode) "emacs")
;;                (t "user"))))
;;  (setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)



;; go mode
(add-to-list 'load-path "~/.emacs.d/")
(require 'go-mode-autoloads)

(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go test -i && go test"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
)
(add-hook 'go-mode-hook 'my-go-mode-hook)


(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)
