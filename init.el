;;; init.el

;;; Commentary:

;;; My Emacs configuration file.

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
  :config
  (load-theme 'modus-operandi)
  :custom
  (modus-themes-org-blocks 'tinted-background)
  (modus-themes-common-palette-overrides
    '((fnname blue-cooler)
      (string green)
      (variable cyan)
      (cursor magenta-faint)
      (bg-region bg-magenta-subtle)
      (fg-region unspecified)
      (border-mode-line-active unspecified)
      (border-mode-line-inactive unspecified)
      (bg-mode-line-active bg-blue-intense)
      (fg-mode-line-active fg-main))))


;; OS-Specific Configuration ------------------------------------

(if (eq system-type 'gnu/linux)
    (load "~/.emacs.d/lisp/linux-init.el"))

(if (eq system-type 'windows-nt)
    (setq ring-bell-function 'ignore))

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

(use-package org-contrib)
(require 'org-depend)

(with-eval-after-load 'org
  
  (setq org-startup-indented t)

  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  
  (setq org-agenda-files '("d:/Documents/Org/tasks.org"
			   "d:/Documents/Org/projects.org"
			   "d:/Documents/Org/inbox.org"
			   "d:/Documents/Org/calendar.org"))

  (setq org-archive-location "d:/Documents/Org/archive/archive23.org::")

  (setq org-refile-targets '(("d:/Documents/Org/tasks.org" :level . 1)
			     ("d:/Documents/Org/calendar.org" :level . 1)
			     ("d:/Documents/Org/projects.org" :maxlevel . 2)))

  (setq org-agenda-custom-commands
	'(("n" "Agenda and NEXT actions"
	   ((agenda "")
	    (todo "NEXT")))))


  (setq org-agenda-window-setup 'only-window)

  (setq denote-org-capture-specifier "%l\n%i\n%?")
  
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
				 (file+headline
				  "d:/Documents/Org/inbox.org"
				  "Tasks")
				 "* TODO %i%?")
				("a" "Appointment [calendar]" entry
				 (file+headline
				  "d:/Documents/Org/calendar.org"
				  "Calendar")
				 "* %i%? \n %^t")
				("n" "New note [denote]" plain
				(file denote-last-path)
				#'denote-org-capture
				:no-save t
				:immediate-finish nil
				:kill-buffer t)
				("r" "Paper [reading-list]" entry
				 (file+headline
				  "d:/Documents/Org/reading-list.org"
				  "Reading List")
				 "* %i%?"))))


(defun dh/org-insert-trigger ()
  "Automatically insert chain-find-next trigger when entry becomes NEXT."
  (cond ((equal org-state "NEXT")
         (unless org-depend-doing-chain-find-next
           (org-set-property "TRIGGER" "chain-find-next(NEXT,from-current,priority-up,effort-down)")))
        ((not (member org-state org-done-keywords))
         (org-delete-property "TRIGGER"))))

(add-hook 'org-after-todo-state-change-hook 'dh/org-insert-trigger)
(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

(defun org-mode-inbox ()
  "Open inbox.org to refile loose items."
  (interactive)
  (find-file "d:/Documents/Org/inbox.org"))

(bind-key "C-c a" 'org-agenda)
(bind-key "C-c c" 'org-capture)
(bind-key "C-c r" 'org-roam-capture)
(bind-key "C-c i" 'org-mode-inbox)


;; Programming --------------------------------------------------

;; R
(use-package ess
  :custom
  (ess-R-readline nil)
  (ess-use-eldoc nil)
  (ess-use-flymake nil)
  (inferior-R-args "--no-save")
  :hook
  ('inferior-ess-mode-hook
   setq-local ansi-color-for-comint-mode 'filter))


;; Emacs Lisp
(setq initial-scratch-message
      ";; Emacs LISP *scratch* buffer\n\n")

;; Common Lisp
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

;; AutoHotKey
(use-package ahk-mode)


;; Academic Writing  ---------------------------------------------------

(use-package citar
  :custom
  (citar-bibliography '("~/Zotero/references.bib"))
  (org-cite-global-bibliography '("~/Zotero/references.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  :bind
  ("C-c n c o" . citar-open-notes)
  (:map org-mode-map :package org ("C-c n c l" . #'org-cite-insert))
  :hook
  (markdown-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package denote
  :ensure t
  :custom
  (denote-directory (expand-file-name "d:/Documents/Org/notes"))
  (denote-known-keywords '("ecology" "philosophy" "emacs"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-file-type nil)
  (denote-prompts '(title keywords))
  (denote-date-prompt-use-org-read-date t)
  :bind
  ("C-c C-n" . denote)
  ("C-c n n" . denote)
  ("C-c n t" . denote-type)
  ("C-c n l" . denote-link)
  ("C-c n L" . denote-add-links)
  ("C-c n b" . denote-backlinks)
  ("C-c n f" . denote-find-link)
  ("C-c n F" . denote-find-backlink)
  ("C-c n r" . denote-rename-file)
  ("C-c n R" . denote-rename-file-using-front-matter))


(use-package citar-denote
  :init (citar-denote-mode)
  :custom
  (citar-notes-paths '("d:/Documents/Org/notes"))
  (citar-denote-title-format "author-year")
  (citar-denote-subdir t)
  :bind
  ("C-c n c c" . citar-create-note)
  ("C-c n c o" . citar-denote-open-note)
  ("C-c n c d" . citar-denote-dwim)
  ("C-c n c a" . citar-denote-add-citekey)
  ("C-c n c k" . citar-denote-remove-citekey)
  ("C-c n c e" . citar-denote-open-reference-entry)
  ("C-c n c r" . citar-denote-find-reference)
  ("C-c n c f" . citar-denote-find-citation)
  ("C-c n c n" . citar-denote-cite-nocite)
  ("C-c n c m" . citar-denote-reference-nocite))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "pandoc"))

(use-package poly-R)
(add-to-list 'auto-mode-alist
             '("\\.[rR]md\\'" . poly-gfm+r-mode))
(setq markdown-code-block-braces t)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'Rd-mode-hook 'flyspell-mode)


;; PDF and EPUB -------------------------------------------------------

(use-package pdf-tools
  :config
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode)))

(use-package calibredb
  :defer t
  :config
  (setq calibredb-root-dir "d:/Documents/Calibre")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '("d:/Documents/Calibre")))

(use-package nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))


;; Diminish minor modes --------------------------------------------------
(use-package diminish
  :config
  (diminish 'citar-denote-mode)
  (diminish 'eldoc-mode))


;; General Keybindings ---------------------------------------------------

(windmove-default-keybindings 'meta)
(setq org-replace-disputed-keys t)

(bind-key "C-M-g" 'exit-recursive-edit)
