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
(add-to-list
 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/"))
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
     (border-mode-line-active unspecified)
     (border-mode-line-inactive)))
  (modus-operandi-palette-overrides
   '((bg-mode-line-active blue-cooler)
     (fg-mode-line-active bg-dim)
     (bg-region bg-blue-subtle)))
  (modus-vivendi-palette-overrides
   '((bg-region bg-blue-nuanced))))

;; Set a dark or light theme on startup based on time of day.
(if (and (< (nth 2 (decode-time (current-time))) 19)
	 (>= (nth 2 (decode-time (current-time))) 6))
    (load-theme 'modus-operandi)
  (load-theme 'modus-vivendi))

;; ;; Export modus themes to various formats.
;; (require 'modus-themes-exporter)


;; -----------------------------------------------------------------------------
;; OS-Specific Configuration

(when (eq system-type 'gnu/linux)
  (use-package vterm)			; shell for linux
  (use-package vterm-toggle
    :bind
    ("C-c t" . 'vterm-toggle)))

(when (eq system-type 'windows-nt)
  (use-package powershell))		; shell for windows


;; -----------------------------------------------------------------------------
;; General

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

(bind-key "C-M-g" 'exit-recursive-edit) ; a more natural exit command

(unbind-key "C-?") ; formerly undo-redo
(bind-key "C-x M-u" 'undo-redo)

(use-package ctrlf ; more intuitive ISearch, with a nice interface.
  :init
  (ctrlf-mode))

(use-package ediff
  :custom
  (ediff-split-window-function 'split-window-horizontal)
  (ediff-window-setup-function 'ediff-setup-windows-plain))


;; -----------------------------------------------------------------------------
;; Autocompletion

;; This provides a pretty good completion system using only built in tools.
;; It isn't as nice as the packages below, but serves as a usable backup.
;; Note: Does not combine with vertico or corfu, but does work with orderless
;; and marginalia.
;; (require 'native-completion)

(electric-pair-mode)
(setq tab-always-indent 'complete)
(setq tab-first-completion nil)

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
  :config
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode)
  :custom
  (corfu-auto t)
  (corfu-quit-no-match t)
  (corfu-min-width 30)
  :bind
  (:map corfu-map ("SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode))

(use-package corfu-candidate-overlay
  :after corfu
  :config
  (corfu-candidate-overlay-mode +1))

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
			    "* %i%? \n %^t")))

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


;; -----------------------------------------------------------------------------
;; Programming Languages

;; R
(use-package ess
  :custom
  (ess-use-flymake nil)
  (inferior-R-args "--no-save")
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
  ('inferior-ess-mode-hook ; fixes color bug in console
   setq-local ansi-color-for-comint-mode 'filter))

;; Python
(use-package pyvenv)

;; Julia
(use-package julia-mode)

;; Go
(use-package go-mode)

;; J
(use-package j-mode
  :custom
  (j-console-cmd "~/J/j9.4/jconsole.sh")
  :mode ("\\.ij[rstp]$" . j-mode))

;; Common Lisp
;; slime-helper.el takes a few seconds to load, so I bind it to a command
;; instead of loading it on startup.

(defun dh/load-slime-helper ()
  "Perform the setup for Common Lisp development with SLIME."
  (interactive)
  (setq-default inferior-lisp-program "sbcl")
  (load (expand-file-name "~/quicklisp/slime-helper.el"))) ;melpa


;; -----------------------------------------------------------------------------
;; Writing

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
  ("C-c b b" . citar-insert-citation)
  :hook
  (org-mode . citar-capf-setup)
  (markdown-mode . citar-capf-setup))

(use-package denote
  ;; quickly take notes in markdown format.
  :custom
  (denote-directory (file-name-as-directory "~/Notes"))
  (denote-silo-extras--directories '("~/Notes/work" "~/Notes/chess"))
  (denote-file-type 'markdown-yaml)
  ;; (denote-file-type nil) ; defaults to org
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

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :custom
  (markdown-command "pandoc"))

(use-package poly-R
  :mode ("\\.[rR]md\\'" . poly-gfm+r-mode)
  :custom
  (markdown-code-block-braces t)
  (markdown-asymmetric-header t))

(add-hook 'markdown-mode-hook #'visual-line-mode)
(add-hook 'find-file-hook #'denote-link-buttonize-buffer)


;; -----------------------------------------------------------------------------
;; PDF and EPUB

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


;; -----------------------------------------------------------------------------
;; Eshell

;; Note: I don't really use eshell anymore since 'vterm' is wayyy better.

;; This is necessary since the 'clear' command sends any current input
;; on the command line before clearing for some reason. The following
;; code renders the expected C-l behavior of most shells.

;; (declare-function eshell-kill-input 'eshell)
;; (declare-function eshell-send-input 'eshell)
;; (declare-function eshell-bol        'eshell)

;; (defun dh/run-this-in-eshell (cmd)
;;     "Run the command 'CMD' in eshell."
;;     `(goto-char (point-max))
;;     (eshell-kill-input)
;;     (message (concat "Running in Eshell: " cmd))
;;     (insert cmd)
;;     (eshell-send-input)
;;     `(goto-char (point-max))
;;     (eshell-bol)
;;     (yank))

;; (add-hook 'eshell-mode-hook
;; 	  (lambda ()
;; 	    (interactive)
;; 	    (bind-key* "C-l" (dh/run-this-in-eshell "clear 1") eshell-mode-map)))


;; -----------------------------------------------------------------------------
;; Start up time

;; Esup provides start up time profiling. See also the start up timer message
;; function in early-init.el.

;; (use-package esup
;;     :custom
;;     (esup-depth 0))

;; The garbage collection threshold is set very low in early-init to reduce
;; start up time. Here we return it to a reasonable value.
(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here
