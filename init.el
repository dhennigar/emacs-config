;; init.el

(load "~/.emacs.d/lisp/straight-bootstrap.el" nil t)


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

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "/usr/bin/pandoc")
  :config (flyspell-mode)
  )

;; Linter
; note this passes an R snippet to the lintr package.
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



;;; Completion System

(electric-pair-mode)

(use-package company
  :init (global-company-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init (marginalia-mode))

(use-package vertico
  :init (vertico-mode))

(use-package embark)

(use-package citar-embark
  :after (citar embark)
  :no-require
  :config (citar-embark-mode))

;;; Citation Manager

(use-package citar
  :custom
  (citar-bibliography '("~/Documents/Ref/Ref.bib"))
  :hook
  (markdown-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))
