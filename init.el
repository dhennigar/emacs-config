;; init.el

;; bootstrap straight.el and load use-package

(define-obsolete-variable-alias
  'native-comp-deferred-compilation-deny-list
  'native-comp-jit-compilation-deny-list
  "Renamed in emacs#95692f6")
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

;; theme settings
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

;; quickly create uniquely named notes.
(use-package denote
  :custom ((denote-directory "~/Documents/Notes/")
	   (denote-keywords '("emacs", "ecology", "salmon", "botany", "R", "FRE", "linux"))))  

;; bibtex citations in org-mode
;;(use-package helm-bibtex
;;  :custom ((bibtex-completion-bibliography "~/Zotero/zotero.bib") ;; this is not correct for Windows! (Unless I move it)
;;	   (bibtex-completion-pdf-field "File"))
;;  )

;; backups are stored in the system temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; autopairs
(setq electric-pair-mode t)

;; org-mode settings

;; org-mode settings
(with-eval-after-load 'org       
  (setq org-startup-indented t)
  (add-hook 'org-mode-hook #'visual-line-mode))

;; bibtex citations in org-mode
(use-package helm-bibtex
  :custom ((bibtex-completion-bibliography "~/Zotero/zotero.bib")
	   (bibtex-completion-pdf-field "File"))
  )

