;;; init.el --- My Emacs configuration file

;; Copyright (c) 2022-2023 Daniel Hennigar

;; Author: Daniel Hennigar
;; URL: https://github.com/dhennigar/emacs-config
;; Package-Requires: ((emacs "28.2"))

;; This file is NOT part of GNU Emacs

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 or any later version.

;; This file is distributed in the hope that it will be usefule,
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; My Emacs configuration file.

;;; Code:

;; Package Management -------------------------------------------

(require 'package)
(add-to-list 'package-archives
	     '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t
      use-package-expand-minimally t)


;; Aesthetics ---------------------------------------------------

(pixel-scroll-precision-mode)

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
    ((use-package vterm)
     (use-package vterm-toggle
       :bind (("C-c v" . 'vterm-toggle)
	      :map vterm-mode-map
	      ("C-c v". 'vterm-toggle)))))

(if (eq system-type 'windows-nt)
    (setq ring-bell-function 'ignore))


;; Custom file --------------------------------------------------

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


;; Midnight mode ------------------------------------------------

(require 'midnight)
(midnight-delay-set 'midnight-delay 1800)


;; Autocompletion -----------------------------------------------

(electric-pair-mode)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(use-package marginalia
  :init (marginalia-mode))

(use-package vertico
  :init (vertico-mode))

(use-package corfu
  :custom
  (corfu-auto-delay 1.0)
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
  
  ;; To-do lists and agendas
  
  (setq org-todo-keywords
	'((sequence
	   "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  
  (setq org-agenda-files '("~/Documents/Org/raincoast.org"
			   "~/Documents/Org/masters.org"
			   "~/Documents/Org/personal.org"
			   "~/Documents/Org/inbox.org"
			   "~/Documents/Org/calendar.org"))

  (setq org-archive-location "~/Documents/Org/archive/archive23.org::")

  (setq org-refile-targets
	'((nil :maxlevel . 9)
          (org-agenda-files :maxlevel . 9)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)
  
  (setq denote-org-capture-specifier "%l\n%i\n%?")
  
  (setq org-capture-templates '(("t" "Task" entry
				 (file+headline
				  "~/Documents/Org/inbox.org"
				  "Tasks")
				 "* TODO %i%?")
				("a" "Appointment" entry
				 (file+headline
				  "~/Documents/Org/calendar.org"
				  "Calendar")
				 "* %i%? \n %^t")
				("n" "Note" plain
				(file denote-last-path)
				#'denote-org-capture
				:no-save t
				:immediate-finish nil
				:kill-buffer t)
				("p" "Planner" entry
				 (file "~/Documents/Org/planner.org")
				 "* [%] %t \n - [ ] %?")
				("r" "Reading List" entry
				 (file+headline
				  "~/Documents/Org/reading-list.org"
				  "Reading List")
				 "* %i%?"))))

;; Aesthetics
  
(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook #'auto-fill-mode)
(setq org-indent-indentation-per-level 1)
(setq org-startup-indented t)
(setq org-hide-emphasis-markers t)

;; Keybindings
  
(bind-key "C-c a" 'org-agenda)
(bind-key "C-c c" 'org-capture)



;; Programming --------------------------------------------------

;; Flymake
(use-package flymake
  :custom (flymake-no-changes-timeout nil))

;; Eldoc
(use-package eldoc
  :custom
  (eldoc-idle-delay 5))

;; R
(use-package ess
  :custom
  (ess-R-readline nil)
  (inferior-R-args "--no-save")
  (ess-R-font-lock-keywords
   '((ess-R-fl-keywor~modifiers . t)
     (ess-R-fl-keywor~fun-defs . t)
     (ess-R-fl-keywor~keywords . t)
     (ess-R-fl-keywor~assign-ops . t)
     (ess-R-fl-keywor~constants . t)
     (ess-fl-keywor~fun-calls . t)
     (ess-fl-keywor~numbers . t)
     (ess-fl-keywor~operators)
     (ess-fl-keywor~delimiters)
     (ess-fl-keywor~=)
     (ess-R-fl-keywor~F&T . t)
     (ess-R-fl-keywor~%op% . t)))
  (display-buffer-alist
   '(("^\\*R"
      (display-buffer-reuse-window display-buffer-in-side-window)
      (side . right)
      (slot . 1)
      (window-width . 0.5)
      (reusable-windows . nil))
     ("^\\*R Dired"
      (display-buffer-reuse-window display-buffer-in-side-window)
      (side . left)
      (slot . 1)
      (window-height . 0.5)
      (reusable-windows . nil))))
  :hook
  ('ess-mode-hook 'turn-on-pretty-mode)
  ('inferior-ess-mode-hook
   setq-local ansi-color-for-comint-mode 'filter))

;; Emacs Lisp
(setq initial-scratch-message
      ";; Emacs LISP *scratch* buffer\n\n")

;; Common Lisp
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

;; Go
(use-package go-mode)
(use-package go-complete
  :hook
  ('completion-at-point-functions 'go-complete-at-point))

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
  (citar-bibliography '("~/Zotero/references.bib"))
  :bind
  ("C-c n c o" . citar-open-notes)
  (:map org-mode-map :package org ("C-c n c l" . #'org-cite-insert))
  :hook
  (markdown-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package denote
  :ensure t
  :custom
  (denote-directory (expand-file-name "~/Documents/Org/notes"))
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
  :functions citar-denote-mode
  :init (citar-denote-mode)
  :custom
  (citar-notes-paths '("~/Documents/Org/notes"))
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
  :defines markdown-command
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :custom
  (markdown-command "pandoc"))

(use-package poly-R
  :defines markdown-code-block-braces
  :config
  (add-to-list 'auto-mode-alist
               '("\\.[rR]md\\'" . poly-gfm+r-mode))
  :custom
  (setq markdown-code-block-braces t))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (python . t)
   (lisp . t)))

(setq org-confirm-babel-evaluate nil)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'Rd-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

(setq ispell-extra-args '("--sug-mode=ultra"))

;; PDF and EPUB -------------------------------------------------------

(use-package pdf-tools
  :config
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode)))

(use-package calibredb
  :defer t
  :config
  (setq calibredb-root-dir "~/Documents/Calibre")
  (setq calibredb-db-dir
	(expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '("~/Documents/Calibre")))

(use-package nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))


;; Diminish minor modes -----------------------------------------------

(use-package diminish
  :config
  (diminish 'citar-denote-mode)
  (diminish 'eldoc-mode))


;; General Keybindings ------------------------------------------------

(bind-key "C-M-g" 'exit-recursive-edit)

(bind-key "C-c f f" 'flymake-start)
(bind-key "C-c f n" 'flymake-goto-next-error)
(bind-key "C-c f p" 'flymake-goto-prev-error)

(unbind-key "C-M-w") ; since I use this to start my web browser.
(unbind-key "C-M-e") ; since I use this to start an emacs client.

;;; init.el ends here
