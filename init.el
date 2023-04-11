;; init.elpp

(load "~/.emacs.d/lisp/straight-bootstrap.el" nil t)


;; Package Repositories and Use-Package Setup

(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))


;; Aesthetics

(use-package modus-themes
  :custom
  ((modus-themes-org-blocks 'tinted-background)
   (modus-themes-common-palette-overrides
    '(;; Code Keywords
      (fnname blue-cooler)
      (string green)
      (variable cyan)
      ;; Interface Colors
      (cursor magenta-faint)
      (bg-region bg-magenta-subtle)
      (fg-region unspecified)
      (border-mode-line-active unspecified)
      (border-mode-line-inactive unspecified)
      (fg-mode-line-active fg-main)))
   )
  )

(use-package circadian
  :ensure t
  :config
  (setq calendar-latitude 49.32)
  (setq calendar-longitude -128.07)
  (setq circadian-themes '((:sunrise . modus-operandi)
                           (:sunset  . modus-vivendi)))
  (circadian-setup))

(use-package tree-sitter-langs
  :straight t)
(use-package tree-sitter)


;; Git

(use-package magit
  :bind (("C-x g" . magit-status)
	 ("C-x C-g" . magit-status)))


;; Note Taking System

(use-package denote
  :custom 
  (denote-directory "~/Documents/Notes")
  (denote-known-keywords '("fre" "salmon" "plants" "stats" "emacs"))
;  (denote-file-type "text")
  :config
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
  :bind
  ("C-c C-n n" . denote)
  ("C-c C-n o" . denote-open-or-create)
  ("C-c C-n t" . denote-type)
  ("C-c C-n l" . denote-link)
  )

(use-package citar-denote
  :config (citar-denote-mode)
  :custom
  (citar-denote-title-format 'author-year)
  :bind
  ("C-c C-n c" . citar-create-note)
  ("C-c C-n S-c" . citar-denote-open-note)
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

(use-package embark
  :bind ("C-." . embark-act)
  )

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
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
  (citar-bibliography '("~/Zotero/references.bib"))
  :hook
  (markdown-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

 (use-package citar-embark
  :after (citar embark)
  :no-require
  :config (citar-embark-mode)
  )


;; R Programming

(use-package ess)
(defun my-inferior-ess-init ()
  (setq-local ansi-color-for-comint-mode 'filter))
(defun my-ess-init ()
  (setq-local ess-use-flymake nil)
  (tree-sitter-hl-mode))
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
  )

(use-package projectile
  :ensure t
  :config (projectile-global-mode)
  :bind (:map projectile-mode-map
	      ("C-c C-p" . 'projectile-command-map))
  )

;; Linter

; note this passes an R snippet to the lintr package.
;(use-package flycheck
;  :init (add-hook 'after-init-hook #'global-flycheck-mode)
;  :custom ((flycheck-lintr-linters 
;	   "linters_with_defaults(trailing_blank_lines_linter = NULL)")
;	   (flycheck-check-syntax-automatically
;	    '(mode-enabled saved)))
;  )


;; Shell Integration

(when (eq system-type 'gnu/linux)
  ((use-package vterm)
   (use-package vterm-toggle
     :bind (("M-RET" . 'vterm-toggle)
	    :map vterm-mode-map
	    ("M-RET". 'vterm-toggle)))
   )
)

;; Keyboard Shortcuts

(bind-key* "M-<right>" 'windmove-right)
(bind-key* "M-<left>" 'windmove-left)
(bind-key* "M-<up>" 'windmove-up)
(bind-key* "M-<down>" 'windmove-down)
(bind-key* "M-o" 'other-window)
