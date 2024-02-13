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

;; Package repositories
(require 'package)
(add-to-list
 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Declarative package management
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq use-package-expand-minimally t)

;; Selectively hide minor modes from the mode line
(use-package diminish)


;; -----------------------------------------------------------------------------
;; Linux-specific configuration

(when (eq system-type 'gnu/linux)
  ;; A much nicer terminal emulator
  (use-package vterm)
  (use-package vterm-toggle
    :bind
    ("C-c t" . 'vterm-toggle))
  ;; ctags-esque functionality
  (use-package ggtags
    :bind
    (:map ggtags-mode-map
	  ("C-c g s" . 'ggtags-find-other-symbol))))


;; -----------------------------------------------------------------------------
;; Theme settings

;; Set these transition times (in hours) for time-appropriate theme selection.
(defvar dh/evening-transition 19)
(defvar dh/morning-transition 6)

(use-package standard-themes
  :custom
  (standard-themes-disable-other-themes t)
  (standard-themes-mixed-fonts t)
  (standard-themes-common-palette-overrides
   '((comment fg-dim)
     (string fg-added)))
  :bind
  ("<f5>" . standard-themes-toggle))

;; Select a time-of-day-appropriate Standard theme.
(if (and (< (nth 2 (decode-time (current-time))) dh/evening-transition)
	 (>= (nth 2 (decode-time (current-time))) dh/morning-transition))
    (standard-themes-load-light)
  (standard-themes-load-dark))

(defun dh/ef-themes-load-random ()
    "Select a random time-of-day-appropriate Ef theme."
    (interactive)
    (if (and (< (nth 2 (decode-time (current-time))) dh/evening-transition)
	     (>= (nth 2 (decode-time (current-time))) dh/morning-transition))
	(ef-themes-load-random 'light)
      (ef-themes-load-random 'dark)))

(use-package ef-themes
  :custom
  (ef-themes-org-blocks 'tinted-background)
  (ef-themes-mixed-fonts t)
  (ef-themes-common-palette-overrides
   '((comment fg-dim)))
  :bind
  ("<f6>" . dh/ef-themes-load-random)
  :config
  (require 'ef-themes-exporter))

(use-package modus-themes
  :custom
  (modus-themes-org-blocks 'tinted-background)
  (modus-themes-mixed-fonts t)
  (modus-themes-common-palette-overrides
   '((comment fg-dim)
     (string green)
     (border-mode-line-active unspecified)
     (border-mode-line-inactive unspecified)))
  :bind
  ("<f7>" . modus-themes-toggle)
  :config
  (require 'modus-themes-exporter))

;; -----------------------------------------------------------------------------
;; General navigation and interface settings

;; Buffer navigation
(bind-key* "C-x <right>" 'windmove-right)
(bind-key* "C-x <left>"  'windmove-left)
(bind-key* "C-x <up>"    'windmove-up)
(bind-key* "C-x <down>"  'windmove-down)
(use-package buffer-move
  :bind
  ("C-x S-<right>" . 'buf-move-right)
  ("C-x S-<left>" . 'buf-move-left)
  ("C-x S-<up>" . 'buf-move-up)
  ("C-x S-<down>" . 'buf-move-down))

;; Change some unintuitive keybindings
(bind-key "C-M-g" 'exit-recursive-edit) ; a more natural exit command
(unbind-key "C-?") ; formerly undo-redo
(bind-key "C-x M-u" 'undo-redo)

;; Superior search tool
(use-package ctrlf
  :init
  (ctrlf-mode))

;; Compare files in two buffers
(use-package ediff
  :custom
  (ediff-split-window-function 'split-window-horizontal)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

;; Show available key sequence completions with C-h
(use-package which-key
  :custom
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay 10000)
  (which-key-idle-secondary-delay 0.05)
  :init
  (which-key-mode))

;; -----------------------------------------------------------------------------
;; Autocompletion

;; ;; `NATIVE-COMPLETION' provides a pretty good completion system using
;; ;; only built-in tools. It isn't as nice as the packages below, but serves
;; ;; as a usable backup.
;; (require 'native-completion)

;; Basic autocompletion ergonomics
(electric-pair-mode)
(setq tab-always-indent 'complete)
(setq tab-first-completion nil)

;; Space delimited orderless selection filtering
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

;; Provide useful details in completion buffers
(use-package marginalia
  :init (marginalia-mode))

;; Provide completion candidates in a vertical list
(use-package vertico
  :custom
  (vertico-count 8)
  :init (vertico-mode))

;; On-the-fly completion candidate overlay
(use-package corfu
  :config
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.5)
  (corfu-quit-no-match t)
  (corfu-min-width 30)
  (corfu-popupinfo-delay nil)
  :bind
  (:map corfu-map
	("M-SPC" . corfu-insert-separator)
	("<f1>" . corfu-popupinfo-toggle)
	("<f2>" . corfu-popupinfo-location))
  :init
  (global-corfu-mode))

;; Quick custom completions
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

(defvar org-directory (expand-file-name "~/org"))

(use-package org
  :config
  (advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)
  :hook
  ('org-mode . 'visual-line-mode)
  :custom
  (org-return-follows-link t)
  (org-todo-keywords
   '((sequence
      "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-agenda-files (list org-directory))
  (org-archive-location
   (concat (file-name-as-directory org-directory) "archive/archive23.org::"))
  (org-refile-targets
	'((nil :maxlevel . 9)
          (org-agenda-files :maxlevel . 9)))
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path t)
  (org-capture-templates '(("t" "Task" entry
			    (file "inbox.org")
			    "* TODO %i%?")
			   ("a" "Appointment" entry
			    (file+headline "calendar.org" "Appointments")
			    "* %i%? \n %^t")
			   ("n" "New note (with Denote)" plain
			    (file denote-last-path)
			    #'denote-org-capture
			    :no-save t
			    :immediate-finish nil
			    :kill-buffer t
			    :jump-to-captured t)))
  (org-agenda-remove-tags t)
  (org-agenda-custom-commands '(("u" "School" tags-todo "ubc")
				("r" "Raincoast" tags-todo "raincoast")
				("p" "Personal" tags-todo "personal")))
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
  (fill-prefix "") ; fixes some bug with pressing RET in org buffers.
  :bind
  ("C-c a" . 'org-agenda)
  ("C-c c" . 'org-capture)
  ("C-c i" . (lambda ()
	       (interactive)
	       (find-file "~/org/inbox.org")))
  ("C-c p" . (lambda ()
	       (interactive)
	       (find-file "~/org/planner.org"))))


;; -----------------------------------------------------------------------------
;; Programming

;; On-the-fly linter
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

;; Documentation on hover
(use-package eldoc
  :diminish
  :custom
  ;; A small delay in Eldoc prevents it from stepping on my typing.
  (eldoc-idle-delay 2.5))


;; -----------------------------------------------------------------------------
;; R

;; TODO: `ess-r-mode-map' is not defined in polymode. How can I use my ESS
;;       keybindings in polymode (e.g., for assignment or pipes)?

;; (defalias 'dh/ess-insert-pipe
;;    (kmacro "SPC % > C-e <return>"))

(use-package ess
  :custom
  (ess-use-flymake nil)
  (inferior-R-args "--no-save")
  (ess-R-font-lock-keywords
   '((ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:constants . t)
     (ess-fl-keyword:fun-calls . t)
     (ess-fl-keyword:numbers)
     (ess-fl-keyword:operators)
     (ess-fl-keyword:delimiters)
     (ess-fl-keyword:=)
     (ess-R-fl-keyword:F&T . t)
     (ess-R-fl-keyword:%op% . t)))
  ;; :bind
  ;; (:map ess-r-mode-map
  ;; 	("C-," . ess-insert-assign)
  ;; 	("C-." . dh/ess-insert-pipe))
  :hook
  ('ess-mode-hook 'turn-on-pretty-mode)
  ('inferior-ess-mode-hook ; fixes color bug in console
   setq-local ansi-color-for-comint-mode 'filter))


;; -----------------------------------------------------------------------------
;; Common Lisp

;; slime-helper.el takes a few seconds to load, so I bind it to a command
;; instead of loading it on startup.

(defun dh/load-slime-helper ()
  "Perform the setup for Common Lisp development with SLIME."
  (interactive)
  (setq-default inferior-lisp-program "sbcl")
  (load (expand-file-name "~/quicklisp/slime-helper.el"))) ;melpa


;; -----------------------------------------------------------------------------
;; Writing tools (Note-taking and Markdown documents)

;; On-the-fly spell checker
(use-package flyspell
  :ensure nil
  :diminish
  :hook
  ('text-mode-hook 'flyspell-mode)
  ('Rd-mode-hook 'flyspell-mode)
  ('org-mode-hook 'flyspell-mode))

;; Spell checking tool
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

;; Citation manager (integrates with Zotero via better-bibtex)
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
  ("C-c b b" . citar-insert-citation)
  :hook
  (org-mode . citar-capf-setup)
  (markdown-mode . citar-capf-setup))

;; Notes management (implements custom file-naming format for organization)
(use-package denote
  :custom
  (denote-directory (file-name-as-directory "~/Notes"))
  (denote-silo-extras--directories '("~/Notes/work" "~/Notes/chess"))
  (denote-file-type 'markdown-yaml)
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-prompts '(title keywords))
  (denote-date-prompt-use-org-read-date t)
  :bind
  ("C-c d d" . denote)
  ("C-c d o" . denote-open-or-create)
  ("C-c d l" . denote-link)
  ("C-c d L" . denote-add-links)
  ("C-c d b" . denote-backlinks)
  ("C-c d f" . denote-find-link)
  ("C-c d F" . denote-find-backlink)
  ("C-c d r" . denote-rename-file))

;; Edit markdown documents
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :custom
  (markdown-command "pandoc"))

;; Edit Rmarkdwon documents
(use-package poly-R
  :mode ("\\.[rR]md\\'" . poly-gfm+r-mode)
  :custom
  (markdown-code-block-braces t)
  (markdown-asymmetric-header t))

;; Cleanup hooks when opening markdown files
(add-hook 'markdown-mode-hook #'visual-line-mode)
(add-hook 'find-file-hook #'denote-link-buttonize-buffer)


;; -----------------------------------------------------------------------------
;; PDF and EPUB

;; Built-in PDF reader
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook
  ('pdf-view-mode-hook  . 'pdf-view-themed-minor-mode))

;; Built in EPUB reader
(use-package nov
  :mode
  (("\\.epub\\'" . nov-mode))
  :custom
  (nov-unzip-program "tar")
  (nov-unzip-args '("-xC" directory "-f" filename)))

;; -----------------------------------------------------------------------------
;; Start-up profiling

;; See the start-up timer in `early-init.el'

;; ;; Profile start-up time on a per-package basis
;; (use-package esup
;;     :custom
;;     (esup-depth 0))

;; The garbage collection threshold is set very low in `early-init.el' to reduce
;; start-up time. Here we return it to a reasonable value.
(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here
