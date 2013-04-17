;; -*- mode: Emacs-Lisp -*-

(require 'cl)

;; ========================== PACKAGES ===========================

(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar tad-packages '(ac-nrepl ac-slime auto-complete
  clojure-mode clojurescript-mode emacs-eclim flymake-cursor
  flymake-jslint go-mode idle-highlight-mode magit nrepl popup
  org-jekyll rainbow-mode rfringe yasnippet))

(defun tad-packages-installed-p ()
  (loop for p in tad-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

(unless (tad-packages-installed-p)
  ;; check for new packages (package version)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p tad-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; ===========================  GLOBAL ===========================

;(ido-mode t)

;; helm
(helm-mode 1)

;; powerline
(powerline-default)

(set-default 'cursor-type 'bar)

;; clean up backup/auto-save files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; company
(require 'company)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)

(defun indent-or-complete ()
  (interactive)
  (if (looking-at "\\_>")
      (company-complete-common)
    (indent-according-to-mode)))

(global-set-key "\t" 'indent-or-complete)

(defun iy-tab-noconflict ()
  (let ((command (key-binding [tab]))) ; remember command
    (local-unset-key [tab]) ; unset from (kbd "<tab>")
    (local-set-key (kbd "TAB") command))) ; bind to (kbd "TAB")

;; helm
(require 'helm-config)
(helm-mode 1)

;; =========================  PROGRAMMING  =========================

;; -- go --
(defun my-go-mode-hook ()
  (setq c-basic-offset 4)
  (setq c-indent-level 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
)
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; -- java --
(require 'eclim)
(require 'eclimd)

(setq eclim-auto-save t)
(global-eclim-mode)

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

(add-hook 'java-mode-hook 'iy-tab-noconflict)

;; ============================= OTHER ==============================

;; -- blogging --

(require 'org-publish)
(require 'org-jekyll)
(add-to-list 'org-publish-project-alist
	'("blog-org"
	 :base-directory "~/Documents/blog/org/"
	 :base-extension "org"
	 :recursive t

	 ;; org publish options
	 :publishing-directory "~/Documents/blog/jekyll/"
	 :publishing-function org-publish-org-to-html

	 ;; org-jekyll options
	 :blog-publishing-directory "~/Documents/blog/jekyll/"
	 :site-root "http://tadfisher.com"
	 :jekyll-sanitize-permalinks t

	 ;; html options
	 :section-numbers nil
	 :headline-levels 4
	 :table-of-contents t
	 :auto-index nil
	 :auto-preamble nil
	 :body-only t
	 :auto-postamble nil))

(add-to-list 'org-publish-project-alist
	     '("blog-static"
	       :base-directory "~/Documents/blog/static/"
	       :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	       :publishing-directory "~/Documents/blog/jekyll/public/static/"
	       :recursive t
	       :publishing-function org-publish-attachment))

(add-to-list 'org-publish-project-alist
	     '("blog" :components ("blog-org" "blog-static")))



;; -- javascript --
;; ===========================  CUSTOM  ============================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-engine (quote xetex))
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (adwaita)))
 '(eclim-java-android-documentation-root "\"/opt/android-sdk/docs/\"")
 '(eclim-java-documentation-root "/usr/share/doc/java6/")
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(org-export-html-toplevel-hlevel 1)
 '(safe-local-variable-values (quote ((eval when (and (buffer-file-name) (file-regular-p (buffer-file-name)) (string-match-p "^[^.]" (buffer-file-name))) (emacs-lisp-mode)))))
 '(scss-compile-at-save nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "microsoft" :slant normal :weight normal :height 98 :width normal))))
 '(header-line ((t (:inherit mode-line :background "gray" :foreground "black" :box nil :weight bold))))
 '(hl-line ((t (:background "gainsboro"))))
 '(mode-line ((t (:background "SlateGray2" :foreground "black" :box nil))))
 '(mode-line-inactive ((t (:inherit mode-line :background "dark slate gray" :foreground "#C6C6C6" :box nil :weight normal))))
 '(powerline-active1 ((t (:inherit mode-line :background "light slate gray" :foreground "white" :box nil))))
 '(powerline-active2 ((t (:inherit mode-line :background "dark slate gray" :foreground "white" :box nil))))
 '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "dark slate gray"))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "dark slate gray")))))
