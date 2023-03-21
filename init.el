;; init.el

;; add my customizations to the load path
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

;; custom emacs theme settings
(use-package modus-themes
  :init (load-theme 'modus-operandi)
  :custom (modus-operandi-palette-overrides nil)
  :bind ("C-c t" . modus-themes-toggle))

;; emacs speaks statistics and rmarkdown abilities
(use-package ess)
(defun my-inferior-ess-init ()
  (setq-local ansi-color-for-comint-mode 'filter))
(defun my-ess-init ()
  (setq-local ess-use-flymake nil)
  (flycheck-mode))
(add-hook 'ess-mode-hook 'my-ess-init)
(add-hook 'inferior-ess-mode-hook 'my-inferior-ess-init)

;; support for Rmd with code execution and knitting/sweaving.
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
  :custom ((denote-directory (expand-file-name "~/Notes/"))
	   (denote-keywords '("emacs", "ecology", "salmon", "botany", "R", "FRE", "linux"))))  

;; backups are stored in the system temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; autopairs
(setq electric-pair-mode t)

