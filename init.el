;; init.el

(load "~/.emacs.d/lisp/straight-bootstrap.el" nil t)


;; Aesthetics

(use-package modus-themes
  :init (load-theme 'modus-operandi)
  :bind ("C-c t" . modus-themes-toggle))

(use-package tree-sitter-langs)
(use-package tree-sitter)


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

(use-package poly-R
  )
(add-to-list 'auto-mode-alist
             '("\\.[rR]md\\'" . poly-gfm+r-mode))
(setq markdown-code-block-braces t)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "/usr/bin/pandoc")
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


;; Note Taking System

(use-package denote
  :custom 
  (denote-directory "~/Documents/Notes")
  (denote-known-keywords '("fre" "emacs" "salmon" "plants"))
  (denote-file-type "text")
  :config
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
  )

(use-package citar-denote
  :init (citar-denote-mode)
  :custom
  (citar-denote-title-format '("author-year"))
  )


;; Completion System

(electric-pair-mode)

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
  :config (citar-embark-mode)
  )

(use-package corfu
  ;; Optional customizations
   :custom
   (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
   (corfu-auto t)                 ;; Enable auto completion
   (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
   (setq read-extended-command-predicate
         #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))


;; Citation Manager

(use-package citar
  :custom
  (citar-bibliography '("~/Documents/References/References.bib"))
  :hook
  (markdown-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))
