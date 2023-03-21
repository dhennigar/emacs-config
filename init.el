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

;; better git integration
(use-package magit
  :bind (("C-x g" . magit-status)
	 ("C-x C-g" . magit-status)))

;; autopairs
(electric-pair-mode)

;; lua-mode
(use-package lua-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
