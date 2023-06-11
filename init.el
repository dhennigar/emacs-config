;;; init.el

;;; Commentary:

;; Emacs initialization file. Here I load and configure several packages which extend standard Emacs functionality.
;; My core workflow includes R programming for data analysis, academic writing in Markdown with poly-mode (.Rmd files),
;; and my life in plain text via org-mode.

;;; Code:


;; Package Management -------------------------------------------

(load "~/.emacs.d/lisp/straight-bootstrap.el" nil t)

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


;; OS-Specific Configuration ------------------------------------

(if (eq system-type 'windows-nt)
    (load "~/.emacs.d/lisp/windows-init.el")
  (load "~/.emacs.d/lisp/linux-init.el"))


;; Aesthetics ---------------------------------------------------

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
      (bg-mode-line-active bg-blue-intense)
      (fg-mode-line-active fg-main)))))

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



;; Autocompletion -----------------------------------------------

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

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)
  (setq tab-always-indent 'complete))


;; Notes --------------------------------------------------------

(use-package denote
  :custom 
  (denote-directory (concat documents-directory "Notes"))
  (denote-known-keywords '("fre" "salmon" "plants" "stats" "emacs"))
  (denote-file-type "text")
  :config
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
  :bind
  ("C-c C-d" . denote)
  ("C-c d" . denote-open-or-create)
  ("C-c M-d" . denote-type)
  ("C-c C-l" . denote-link))

(use-package citar-denote
  :config (citar-denote-mode)
  :custom
  (citar-denote-title-format "author-year")
  (citar-notes-path  (concat documents-directory "Notes"))
  :bind
  ("C-c C-b" . citar-create-note)
  ("C-c b" . citar-denote-open-note))

(use-package citar
  :custom
  (citar-bibliography '("~/Zotero/references.bib"))
  :hook
  (markdown-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package citar-embark
  :after (citar embark)
  :no-require
  :config (citar-embark-mode))


;; Programming --------------------------------------------------

;; Git
(use-package magit
  :bind (("C-x g" . magit-status)
	 ("C-x C-g" . magit-status)))

;; LSP
(use-package eglot)

;; Linting
(use-package flymake
  :custom
  ((flymake-wrap-around 1)
   (flymake-no-changes-timeout 5)))

;; R (including .Rmd)
(use-package ess)
(defun my-inferior-ess-init ()
  (setq-local ansi-color-for-comint-mode 'filter))
(defun my-ess-init ()
  (eglot-ensure)
  (tree-sitter-hl-mode))
(add-hook 'ess-mode-hook 'my-ess-init)
(add-hook 'inferior-ess-mode-hook 'my-inferior-ess-init)

(use-package poly-R)
(add-to-list 'auto-mode-alist
             '("\\.[rR]md\\'" . poly-gfm+r-mode))
(setq markdown-code-block-braces t)

;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "pandoc"))

(use-package pdf-tools)


;; Org-Mode -----------------------------------------------------

(with-eval-after-load 'org       
  (setq org-startup-indented t)
  (setq org-agenda-files
        (directory-files-recursively
         orgfiles-directory "\\.org$")))
(add-hook 'org-mode-hook #'visual-line-mode)

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)))


;; Keybindings

(windmove-default-keybindings 'meta)
(setq org-replace-disputed-keys t)
