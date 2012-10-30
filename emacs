;; -*- mode: Lisp -*-

(require 'cl)

;; ========================== PACKAGES ===========================

(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar tad-packages
  '(ac-nrepl ac-slime auto-complete clojure-mode clojurescript-mode
	     emacs-eclim flymake-cursor flymake-jslint flymake-lintnode
	     go-mode idle-highlight-mode magit nrepl popup rainbow-mode
	     rfringe yasnippet))

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

(ido-mode t)

(set-default 'cursor-type 'bar)

;; clean up backup/auto-save files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; auto-complete-mode
(require 'auto-complete-config)
(ac-config-default)
(require 'ac-emacs-eclim-source)
(add-hook 'eclim-mode-hook (lambda ()
			     (add-to-list 'ac-sources 'ac-source-emacs-eclim)
			     (add-to-list 'ac-sources 'ac-source-emacs-eclim)))

(defun iy-tab-noconflict ()
  (let ((command (key-binding [tab]))) ; remember command
    (local-unset-key [tab]) ; unset from (kbd "<tab>")
    (local-set-key (kbd "TAB") command))) ; bind to (kbd "TAB")

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

;; -- javascript --
(require 'flymake-lintnode)
(setq lintnode-location "~/git/lintnode")
;(setq lintnode-jslint-excludes (list 'nomen 'plusplus 'onevar 'white))
(add-hook 'js-mode-hook (lambda () lintnode-hook))

;; ===========================  CUSTOM  ============================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (adwaita)))
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(safe-local-variable-values (quote ((eval when (and (buffer-file-name) (file-regular-p (buffer-file-name)) (string-match-p "^[^.]" (buffer-file-name))) (emacs-lisp-mode)))))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "microsoft" :slant normal :weight normal :height 98 :width normal))))
 '(header-line ((t (:inherit mode-line :background "gray" :foreground "black" :box nil :weight bold)))))
