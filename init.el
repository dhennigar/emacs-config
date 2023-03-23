;; init.el

(load "~/.emacs.d/lisp/straight-bootstrap.el" nil t)

;; Completion

(electric-pair-mode)
(use-package company)


;; Aesthetics

(use-package modus-themes
  :init (load-theme 'modus-operandi)
  :custom (modus-operandi-palette-overrides nil)
  :bind ("C-c t" . modus-themes-toggle))

(use-package tree-sitter
  :config (tree-sitter-hl-mode)
  :requires (tree-sitter-langs)
  )


;; R Programming

(use-package ess)
(defun my-inferior-ess-init ()
  (setq-local ansi-color-for-comint-mode 'filter))
(defun my-ess-init ()
  (setq-local ess-use-flymake nil)
  (flycheck-mode)
  (company-mode))
(add-hook 'ess-mode-hook 'my-ess-init)
(add-hook 'inferior-ess-mode-hook 'my-inferior-ess-init)

(use-package poly-R)
(add-to-list 'auto-mode-alist
             '("\\.[rR]md\\'" . poly-gfm+r-mode))
(setq markdown-code-block-braces t)


;; Linter
(use-package flycheck
  :custom (flycheck-lintr-linters 
	   "linters_with_defaults(trailing_blank_lines_linter = NULL)"))

;; Git
(use-package magit
  :bind (("C-x g" . magit-status)
	 ("C-x C-g" . magit-status)))


;;; Note Taking System

(use-package denote
  :custom 
  (denote-directory "~/Documents/Notes")
  (denote-known-keywords '("fre" "emacs" "salmon" "plants"))
  (denote-file-type "text")
  :config
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
  )
