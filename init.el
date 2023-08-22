;;; init.el --- My Emacs configuration file

;; Copyright (c) 2022-2023 Daniel Hennigar

;; Author: Daniel Hennigar
;; URL: https://github.com/dhennigar/emacs-config
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 or any later version.

;; This file is distributed in the hope that it will be useful,
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; I use Emacs as an integrated research environment (IRE).  With this
;; configuration I can complete most of my academic research within Emacs,
;; from lit-review and bibliographic note-taking, to data analysis in R
;; and report/manuscript outlining, writing, and editing.  I can also manage
;; my e-book library and read books in EPUB format.

;;; Code:

;; -----------------------------------------------------------------------------
;; Package Management

(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq use-package-expand-minimally t)

(use-package diminish)


;; -----------------------------------------------------------------------------
;; Theme settings

(use-package modus-themes
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

(if (and (< (nth 2 (decode-time (current-time))) 20)
	 (>= (nth 2 (decode-time (current-time))) 6))
    (load-theme 'modus-operandi)
  (load-theme 'modus-vivendi))


;; -----------------------------------------------------------------------------
;; OS-Specific Configuration

;; (if (eq system-type 'gnu/linux)
;;  ;; any linux-specific config goes here
;;     )

(if (eq system-type 'windows-nt)
    (setq ring-bell-function 'ignore))


;; -----------------------------------------------------------------------------
;; Autocompletion

(electric-pair-mode)
(setq tab-always-indent 'complete)
(setq tab-first-completion nil)

;; This section renders a fairly usable native completion framework,
;; but it isn't compatible with vertico, so don't enable both. This
;; should be used in lieu of the nice packages below as a backup.

;; (setq completion-auto-help 'always)
;; (setq completion-auto-select 'second-tab)

;; (setq completions-max-height 10)
;; (setq completions-header-format nil)
;; (setq completion-show-help nil)

;; (define-key minibuffer-mode-map (kbd "C-n")
;; 	    #'minibuffer-next-completion)
;; (define-key minibuffer-mode-map (kbd "C-p")
;; 	    #'minibuffer-previous-completion)

;; (define-key completion-in-region-mode-map (kbd "C-n")
;; 	    #'minibuffer-next-completion)
;; (define-key completion-in-region-mode-map (kbd "C-p")
;; 	    #'minibuffer-previous-completion)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(use-package marginalia
  :init (marginalia-mode))


;; These two packages render a nicer looking completion framework
;; both for the minibuffer and in-region. They should not be used
;; in conjunction with the native completion framework code above.

(use-package vertico
  :init (vertico-mode))

(use-package corfu
  :custom
  (corfu-auto nil)
  (corfu-quit-no-match t)
  (corfu-min-width 30)
  :bind
  (:map corfu-map ("SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode)
  :hook
  ('eshell-mode-hook . (lambda ()
			 (setq-local corfu-auto nil)
			 (corfu-mode))))


;; See the abbrev file for available auto-expanding completions.

(use-package abbrev
  :ensure nil
  :diminish
  :custom
  (abbrev-file-name "~/.emacs.d/abbrevs")
  :config
  (dolist (hook '(org-mode-hook
                  markdown-mode-hook
		  Rd-mode-hook
                  text-mode-hook))
    (add-hook hook #'abbrev-mode)))


;; -----------------------------------------------------------------------------
;; Org Mode

;(use-package org-contrib) ; is this needed?

(use-package org
  :hook
  ('org-mode-hook . 'visual-line-mode)
  
  :custom

  ;; General org-mode stuff goes here

  (org-tags-column -80)
  
  ;; This section defines my settings for org-agenda and calendar
  ;; views which form the basis of my personal productivity system.
  
  (org-todo-keywords
   '((sequence
      "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  
  (org-agenda-files '("~/Documents/Org/work.org"
		      "~/Documents/Org/personal.org"
		      "~/Documents/Org/inbox.org"
		      "~/Documents/Org/calendar.org"))

  (org-archive-location "~/Documents/Org/archive/archive23.org::")

  (org-refile-targets
	'((nil :maxlevel . 9)
          (org-agenda-files :maxlevel . 9)))
  
  (org-outline-path-complete-in-steps nil)
  
  (org-refile-use-outline-path t)
  
  (denote-org-capture-specifier "%l\n%i\n%?")
  
  (org-capture-templates '(("t" "Task" entry
			    (file "~/Documents/Org/inbox.org")
			    "* TODO %i%?")
			   ("a" "Appointment" entry
			    (file "~/Documents/Org/calendar.org")
			    "* %i%? \n %^t")
			   ("n" "Note" plain
			    (file denote-last-path)
			    #'denote-org-capture
			    :no-save t :immediate-finish nil :kill-buffer t)
			   ("r" "Reading List" entry
			    (file "~/Documents/Org/reading-list.org")
			    "* %i%?")))

  (org-agenda-remove-tags t)

  (org-agenda-custom-commands '(("o" "Office tasks" tags-todo "office")
				("f" "Field tasks" tags-todo "field")))
  
  ;; Here I define the statutory holidays for British Columbia
  ;; which will appear in my calendar views.
  
  (holiday-local-holidays
	'((holiday-fixed 1 1 "New Year's Day")
	  (holiday-float 2 1 3 "Family Day")
	  (holiday-easter-etc -2 "Good Friday")
	  (holiday-float 5 1 -2 "Victoria Day")
	  (holiday-fixed 6 21 "Indigenous Peoples Day")
	  (holiday-fixed 7 1 "Canada Day")
	  (holiday-float 8 1 1 "BC Day")
	  (holiday-fixed 9 30 "National Day for Truth and Reconcilliation")
	  (holiday-float 10 1 2 "Canadian Thanksgiving")
	  (holiday-fixed 10 31 "Halloween")
	  (holiday-fixed 11 11 "Rememberance Day")
	  (holiday-float 11 4 4 "American Thanksgiving")
	  (holiday-fixed 12 25 "Christmas")))

  ;; turn off all the US and religeous holidays
  (holiday-general-holidays nil)
  (holiday-christian-holidays nil)
  (holiday-islamic-holidays nil)
  (holiday-bahai-holidays nil)
  (holiday-hebrew-holidays nil)
  (holiday-oriental-holidays nil)

  (calendar-holidays holiday-local-holidays)

  ;; See the ob-* use-package declarations below for language-
  ;; specific org-babel set up.
  (org-confirm-babel-evaluate nil)
  
  :bind
  ("C-c o" . (lambda ()
	       (interactive)
	       (find-file "~/Documents/Org")))
  ("C-c i" . (lambda ()
	       (interactive)
	       (find-file "~/Documents/Org/inbox.org")))
  ("C-c a" . 'org-agenda)
  ("C-c c" . 'org-capture))
  
(use-package ob-R
  :ensure nil
  :after (org)
  :commands (org-babel-execute:R))

(use-package ob-lisp
  :ensure nil
  :after (org)
  :commands (org-babel-execute:lisp))


;; -----------------------------------------------------------------------------
;; Programming

;; The following two packages provide linting and documentation on hover

(use-package flycheck
  :hook
  ('ess-mode-hook . flycheck-mode)
  ('emacs-lisp-mode . flycheck-mode)
  :custom
  (flycheck-lintr-linters
   "linters_with_defaults(trailing_blank_lines_linter = NULL, indentation_linter = indentation_linter(indent = 4L))")
  (flycheck-check-syntax-automatically '(save mode-enabled))
  :bind
  ("C-c f f" . 'flycheck-buffer)
  ("C-c f n" . 'flycheck-next-error)
  ("C-c f p" . 'flycheck-previous-error))

(use-package eldoc
  :diminish
  :custom
  ;; A small delay in Eldoc prevents it from stepping on my typing.
  (eldoc-idle-delay 2.5))

;; Language-specific configs

(use-package ess
  :custom
  (ess-use-flymake nil)
  (inferior-R-args "--no-save")
  (ess-R-readline nil) ; I don't remember why this is here.

  ;; syntax hilighting options
  (ess-R-font-lock-keywords
   '((ess-R-fl-keywor~modifiers . t)
     (ess-R-fl-keywor~fun-defs . t)
     (ess-R-fl-keywor~keywords . t)
     (ess-R-fl-keywor~assign-ops . t)
     (ess-R-fl-keywor~constants . t)
     (ess-fl-keywor~fun-calls . t)
     (ess-fl-keywor~numbers . t)
     (ess-fl-keywor~operators . t)
     (ess-fl-keywor~delimiters)
     (ess-fl-keywor~=)
     (ess-R-fl-keywor~F&T . t)
     (ess-R-fl-keywor~%op% . t)))
  
  :hook
  ('ess-mode-hook 'turn-on-pretty-mode)
  ('inferior-ess-mode-hook
   setq-local ansi-color-for-comint-mode 'filter))

;; The slime-helper.el script takes a few seconds to run,
;; so instead of calling it on startup I prefer to bind
;; it to a function that allows me to initialize slime
;; as and when I need it.

(defun dh/load-slime-helper ()
  "Perform the setup for Common Lisp development with SLIME."
  (interactive)
  (setq-default inferior-lisp-program "sbcl")
  (load (expand-file-name "~/quicklisp/slime-helper.el"))) ;melpa

;; I don't actually use Go that much anymore, so I've disabled the support here.
;; Uncomment if the go bug bites again.
;; (use-package go-mode)
;; (use-package go-complete
;;   :hook
;;   ('completion-at-point-functions 'go-complete-at-point))

(use-package ahk-mode) ; haven't explored any config options


;; -----------------------------------------------------------------------------
;; Writing

(setenv "LANG" "en_CA")

(use-package flyspell
  :ensure nil
  :diminish
  :hook
  ('text-mode-hook 'flyspell-mode)
  ('Rd-mode-hook 'flyspell-mode)
  ('org-mode-hook 'flyspell-mode))

(use-package ispell
  :after (flyspell)
  :ensure nil
  :diminish
  :custom
  (ispell-personal-dictionary "~/.hunspell")
  (ispell-cmd-args "-p ~/.hunspell")
  (ispell-program-name "hunspell")
  :bind
  ("<f5>" . 'ispell-word))

(use-package citar
  :custom
  (citar-bibliography '("~/Zotero/references.bib"))
  (org-cite-global-bibliography '("~/Zotero/references.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography '("~/Zotero/references.bib"))
  :bind
  ("C-c b f" . citar-open-files)
  (:map org-mode-map :package org ("C-c b i" . #'org-cite-insert))
  :hook
  (markdown-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package denote
  :custom
  (denote-directory (expand-file-name "~/Documents/Org/notes"))
  (denote-known-keywords '("work" "personal" "emacs"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-file-type nil)
  (denote-prompts '(title keywords))
  (denote-date-prompt-use-org-read-date t)
  :bind
  ("C-c d d" . denote-open-or-create)
  ("C-c d l" . denote-link)
  ("C-c d L" . denote-add-links)
  ("C-c d b" . denote-backlinks)
  ("C-c d f" . denote-find-link)
  ("C-c d F" . denote-find-backlink)
  ("C-c d r" . denote-rename-file)
  ("C-c d R" . denote-rename-file-using-front-matter))

(use-package citar-denote
  :diminish
  :init (citar-denote-mode)
  :custom
  (citar-notes-paths '("~/Documents/Org/notes"))
  (citar-denote-title-format "author-year")
  (citar-denote-subdir t)
  :bind
  ("C-c b b" . citar-create-note)
  ("C-c b o" . citar-denote-open-note)
  ("C-c b k" . citar-denote-add-citekey)
  ("C-c b r" . citar-denote-find-reference)
  ("C-c b C-f" . citar-denote-find-citation))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :custom
  (markdown-command "pandoc"))

(use-package poly-R
  :mode ("\\.[rR]md\\'" . poly-gfm+r-mode)
  :custom
  (markdown-code-block-braces t))


;; -----------------------------------------------------------------------------
;; PDF and EPUB

;; These three packages combine to allow me to view pdfs, epubs,
;; and several other document formats, and manage my e-book library
;; by integrating the Calibre's database.

(use-package pdf-tools
  :defines pdf-view-themed-minor-mode
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook
  ('pdf-view-mode-hook  . 'pdf-view-themed-minor-mode))

(use-package nov
  :mode
  (("\\.epub\\'" . nov-mode))
  :custom
  (nov-unzip-program "tar")
  (nov-unzip-args '("-xC" directory "-f" filename)))

(use-package calibredb
  :custom
  (calibredb-db-dir "~/Documents/Calibre/metadata.db")
  (calibredb-library-alist '("~/Documents/Calibre")))


;; -----------------------------------------------------------------------------
;; Eshell

;; This is necessary since clear command sends any current input
;; on the command line before clearing for some reason. The following
;; code renders the expected C-l behavior of most shells.

(declare-function eshell-kill-input 'eshell)
(declare-function eshell-send-input 'eshell)
(declare-function eshell-bol        'eshell)

(defun run-this-in-eshell (cmd)
    "Run the command 'CMD' in eshell."
    `(goto-char (point-max))
    (eshell-kill-input)
    (message (concat "Running in Eshell: " cmd))
    (insert cmd)
    (eshell-send-input)
    `(goto-char (point-max))
    (eshell-bol)
    (yank))

(add-hook 'eshell-mode-hook (lambda ()
    (interactive)
    (bind-key* "C-l" (run-this-in-eshell "clear 1") eshell-mode-map)))


;; -----------------------------------------------------------------------------
;; Init profiling

;; This is used for profiling the startup time of your init files.
;; Load this config to troubleshoot any future start-up lags.
;;
;; The startup timer message in early-init.el is another tool that
;; is useful for this purpose.

(use-package esup
   :custom
   (esup-depth 0))


;; -----------------------------------------------------------------------------
;; General Keybindings

(bind-key* "C-S-<right>" 'windmove-right)
(bind-key* "C-S-<left>"  'windmove-left)
(bind-key* "C-S-<up>"    'windmove-up)
(bind-key* "C-S-<down>"  'windmove-down)

(bind-key  "C-M-<return>"   'split-window-horizontally)
(bind-key  "C-M-S-<return>" 'split-window-vertically)

(bind-key "C-M-g" 'exit-recursive-edit) ; a more natural exit command

(unbind-key "C-?") ; formerly undo-redo
(bind-key "C-x M-u" 'undo-redo)

(unbind-key "C-M-w") ; since I use this to start my web browser
(unbind-key "C-M-e") ; since I use this to start an emacs client


;; set the garbage collection back to something reasonable
(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here
