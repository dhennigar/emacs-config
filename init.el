;; init.el

;; basic gui settings
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(setq inhibit-splash-screen t)
(setq custom-safe-themes t)

;; default font settings
(add-to-list 'default-frame-alist '(font . "Cascadia Code 10" ))
(add-to-list 'default-frame-alist '(font . "IBM Plex Mono 12" ))

;; package managers
;; bootstrap straight.el and install use-package
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; custom emacs theme settings
(use-package doom-themes
  :config (load-theme 'doom-tomorrow-night))

(defun theme-light-mode ()
  (interactive)
  (disable-theme 'doom-tomorrow-night)
  (load-theme 'doom-tomorrow-day))

(defun theme-dark-mode ()
  (interactive)
  (disable-theme 'doom-tomorrow-day)
  (load-theme 'doom-tomorrow-night))

;; emacs speaks statistics and rmarkdown abilities
(use-package ess)
(defun my-inferior-ess-init ()
  (setq-local ansi-color-for-comint-mode 'filter))
(defun my-ess-init ()
  (setq-local ess-use-flymake nil)
  (flycheck-mode))
(add-hook 'ess-mode-hook 'my-ess-init)
(add-hook 'inferior-ess-mode-hook 'my-inferior-ess-init)

(use-package poly-R)

;; linting (syntax checking)
(use-package flycheck
  :custom (flycheck-lintr-linters 
	   "linters_with_defaults(trailing_blank_lines_linter = NULL)"))

;; synchronous autocompletion
(use-package company
  :config (global-company-mode))

;; tree-sitter support with syntax hilighting.
(use-package tree-sitter)
  :init (global-tree-sitter-mode)
(use-package tree-sitter-langs)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; better git integration
(use-package magit)

;; quickly create uniquely named notes.
(use-package denote
  :custom ((denote-directory (expand-file-name "~/notes/"))
	   (denote-keywords '("emacs", "ecology", "salmon", "botany", "R", "FRE", "linux"))))  

;; buffers are per-frame
(use-package beframe)

;(use-package evil
;  :config (evil-mode))

;; backups are stored in the system temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
