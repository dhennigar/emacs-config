;; init.el

(load "~/.emacs.d/lisp/straight-bootstrap.el" nil t)
(load "~/.emacs.d/lisp/r-programming.el" nil t)
(load "~/.emacs.d/lisp/org-mode-settings.el"nil t)

;; theme settings
(use-package modus-themes
  :init (load-theme 'modus-operandi)
  :custom (modus-operandi-palette-overrides nil)
  :bind ("C-c t" . modus-themes-toggle))

;; linting (syntax checking)
(use-package flycheck
  :custom (flycheck-lintr-linters 
	   "linters_with_defaults(trailing_blank_lines_linter = NULL)"))

;; synchronous autocompletion
(use-package company
  :config (global-company-mode))

;; tree-sitter support with syntax hilighting.
(use-package tree-sitter
  :config (tree-sitter-hl-mode)
  :requires (tree-sitter-langs)
  )
;;(use-package tree-sitter-langs)
;;(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; better git integration
(use-package magit
  :bind (("C-x g" . magit-status)
	 ("C-x C-g" . magit-status)))

;; autopairs
(electric-pair-mode)

;; lua-mode
(use-package lua-mode)

