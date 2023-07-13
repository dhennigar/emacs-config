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
  (setq circadian-themes '(("5:00" . modus-operandi)
                           ("19:00"  . modus-vivendi)))
  (circadian-setup))

(use-package tree-sitter-langs
  :straight t)
(use-package tree-sitter)


;; OS-Specific Configuration ------------------------------------

(if (eq system-type 'windows-nt)
    (load "~/.emacs.d/lisp/windows-init.el")
  (load "~/.emacs.d/lisp/linux-init.el"))


;; Custom file --------------------------------------------------

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


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


;; Org-mode --------------------------------------------------------

(with-eval-after-load 'org
  
  (setq org-startup-indented t)

  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  
  (setq org-agenda-files '("~/Dropbox/org/tasks.org"
			   "~/Dropbox/org/projects.org"
			   "~/Dropbox/org/inbox.org"
			   "~/Dropbox/org/calendar.org"))


  (setq org-archive-location "~/Dropbox/org/archive/archive23.org::")

  (setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

  (setq org-capture-templates '(("t" "Todo [inbox]" entry
				 (file+headline "~/Dropbox/org/gtd/inbox.org" "Tasks")
				 "* TODO %i%?")
				("a" "Appointment [calendar]" entry
				 (file+headline "~/Dropbox/org/gtd/calendar.org" "Calendar")
                               "* %i%? \n %^t"))))

(add-hook 'org-mode-hook #'visual-line-mode)

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Dropbox/org/roam")
  :bind
  (("C-c n r" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(use-package citar
  :custom
  (citar-bibliography '("~/Zotero/references.bib"))
  (org-cite-global-bibliography '("~/Zotero/references.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  :bind
  ("C-c n b" . citar-open-notes)
  (:map org-mode-map :package org ("C-c n l" . #'org-cite-insert))
  :hook
  (markdown-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package citar-org-roam
  :after (citar org-roam)
  :config (citar-org-roam-mode))

(bind-key "C-c a" 'org-agenda)
(bind-key "C-c c" 'org-capture)
(bind-key "C-c r" 'org-roam-capture)


;; Programming --------------------------------------------------

;; Git
(use-package magit
  :bind (("C-c . g" . magit-status)
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


;; Emacs Lisp
(setq initial-scratch-message
      ";; Emacs LISP *scratch* buffer

")


;; Markdown and PDFs --------------------------------------------

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "pandoc"))

(use-package pdf-tools)


;; Keybindings ---------------------------------------------------

(windmove-default-keybindings 'meta)
(setq org-replace-disputed-keys t)

(bind-key "C-M-g" 'exit-recursive-edit)


;; Calibre -------------------------------------------------------

(use-package calibredb
  :defer t
  :config
  (setq calibredb-root-dir "~/Documents/Calibre")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '("~/Documents/Calibre")))


(use-package nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))


;; Multi Media ------ ---------------------------------------------

(use-package emms
  :config
  (emms-all)
  :custom
  (emms-player-list '(emms-player-mpv))
  (emms-info-functions '(emms-info-native))
  :bind
  ("C-c . m" . emms-browser))

