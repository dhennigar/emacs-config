;;; init.el --- My Emacs configuration file

;; Copyright (c) 2022-2024 Daniel Hennigar

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
;; configuration I can complete most of my academic research within Emacs.
;; Supported tasks include literature review and note management; data analysis
;; in the R programming language; report/manuscript outlining, writing, and
;; editing; and project management with the excellent `org-mode'.  This
;; configuration also integrates with Calibre and Zotero for e-book management
;; and bibliographic library management, respectively.

;;; Code:

;; -----------------------------------------------------------------------------
;; Package Management

;; Package repositories
(require 'package)
(add-to-list
 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Declarative package management
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq use-package-expand-minimally t)

;; Selectively hide minor modes from the mode line
;; Available via the use-package keyword `:diminish'
(use-package diminish
  :config
  (diminish 'visual-line-mode))


;; -----------------------------------------------------------------------------
;; Linux-specific configuration

(when (eq system-type 'gnu/linux)
  (use-package vterm)
  (use-package vterm-toggle
    :bind
    ("C-c t" . 'vterm-toggle))
  (use-package ggtags
    :init
    (define-prefix-command 'dh/ggtags-map)
    :bind
    ("C-c g" . 'dh/ggtags-map)
    (:map dh/ggtags-map
	  ("g" . 'ggtags-mode)
	  ("s" . 'ggtags-find-other-symbol))))


;; -----------------------------------------------------------------------------
;; Theme settings

;; Set these transition times (in hours) for time-appropriate theme selection.
(defvar dh/evening-transition 19)
(defvar dh/morning-transition 6)

(use-package standard-themes
  :defer nil
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
(bind-key* "<mouse-8>"   'previous-buffer)
(bind-key* "<mouse-9>"   'next-buffer)
(use-package buffer-move
  :defer nil
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
  :defer nil
  :init
  (ctrlf-mode))

;; Layout settings for diff buffers
(use-package ediff
  :custom
  (ediff-split-window-function 'split-window-horizontal)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

;; Show available key sequence completions with C-h
(use-package which-key
  :defer nil
  :diminish which-key-mode
  :custom
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay 10000)
  (which-key-idle-secondary-delay 0.05)
  :init
  (which-key-mode))

;; Scratch buffers
(use-package scratch
  :bind
  ("C-c s" . scratch))


;; -----------------------------------------------------------------------------
;; Autocompletion

;; ;; `native-completion.el' provides a pretty good completion system using
;; ;; only built-in tools. It isn't as nice as the packages below, but serves
;; ;; as a usable backup.
;; (require 'native-completion)

;; Basic autocompletion ergonomics
(electric-pair-mode)
(setq tab-always-indent 'complete)
(setq tab-first-completion nil)

;; Space delimited orderless selection filtering
(use-package orderless
  :defer nil
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

;; Provide useful details in completion buffers
(use-package marginalia
  :defer nil
  :init (marginalia-mode))

;; Provide completion candidates in a vertical list
(use-package vertico
  :defer nil
  :custom
  (vertico-count 8)
  (vertico-resize nil)
  :init
  (vertico-mode))

;; Persistant search history pairs well with vertico
(use-package savehist
  :defer nil
  :init
  (savehist-mode))

;; On-the-fly completion candidate overlay
(use-package corfu
  :defer nil
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
	("M-SPC" . 'corfu-insert-separator)
	("<f1>" . 'corfu-popupinfo-toggle))
  :init
  (global-corfu-mode))


;; Quick abbreviation expansion
(use-package abbrev
  :defer nil
  :ensure nil ; does not require installation
  :diminish abbrev-mode
  :config
  ;; this tells abbrev mode to look for words which start with "/"
  (abbrev-table-put global-abbrev-table
		    :regexp "\\(?:^\\|[\t\s]+\\)\\(?1:[/].*\\)")
  :custom
  (abbrev-file-name "~/.emacs.d/abbrevs")
  :hook
  (prog-mode . abbrev-mode) ; parent mode for all programming modes
  (tex-mode . abbrev-mode)) ; parent mode for all text modes


;; -----------------------------------------------------------------------------
;; Programming tools

;; Documentation on hover
(use-package eldoc
  :diminish eldoc-mode
  :custom
  ;; A small delay in Eldoc prevents it from stepping on my typing.
  (eldoc-idle-delay 2.0))

;; Flymake, the on-the-fly syntax checker
(use-package flymake
  :ensure nil
  :init (define-prefix-command 'dh/flymake-map)
  :custom
  (flymake-no-changes-timeout 2.0)
  :bind
  ("C-c f" . dh/flymake-map)
  (:map dh/flymake-map
	("f" . flymake-mode)
	("n" . flymake-goto-next-error)
	("p" . flymake-goto-prev-error)
	("b" . flymake-show-buffer-diagnostics)))

;; Projectile, the project-level management system
(use-package projectile
  :config
  (projectile-mode +1)
  :bind (:map projectile-mode-map ("C-c p" . 'projectile-command-map)))

;; Tree-sitter grammars
(setq treesit-language-source-alist
      '((markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(bash "https://github.com/tree-sitter/tree-sitter-bash")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(r "https://github.com/r-lib/tree-sitter-r")))


;; -----------------------------------------------------------------------------
;; R

;; Emacs Speaks Statistics
(use-package ess
  :config
  (defun dh/ess-insert-pipe ()
    "Insert `%>%', trim whitespace, and insert newline with indentation."
    (interactive)
    (unless (ess-inside-string-or-comment-p)
      (delete-horizontal-space)
      (insert " %>% ")
      (ess-roxy-newline)))
  (require 'ess-r-mode)
  (bind-key "<f1>" 'ess-display-help-on-object ess-r-mode-map)
  (bind-key "C-," 'ess-insert-assign ess-r-mode-map)
  (bind-key "C-." 'dh/ess-insert-pipe ess-r-mode-map)
  :custom
  (ess-use-flymake t)
  (ess-r-flymake-linters
   '("trailing_blank_lines_linter = NULL"
     "indentation_linter = indentation_linter(indent = 4L)"))
  (inferior-R-args "--no-save")
  (ess-indent-level 2)
  (ess-R-font-lock-keywords		; selectively highlight syntax
   '((ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:fun-defs)
     (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:constants . t)
     (ess-fl-keyword:fun-calls . t)
     (ess-fl-keyword:numbers)
     (ess-fl-keyword:operators)
     (ess-fl-keyword:delimiters)
     (ess-fl-keyword:=)
     (ess-R-fl-keyword:F&T . t)
     (ess-R-fl-keyword:%op% . t))))

;; Tidyverse-esque data viewer in an Emacs buffer
(use-package ess-view-data
  :after (ess)
  :bind
  (:map ess-mode-map ("C-c v" . 'ess-view-data-print))
  (:map ess-view-data-mode-map
	("f" . ess-view-data-filter)
	("s" . ess-view-data-sort)))


;; -----------------------------------------------------------------------------
;; Common Lisp

;; `slime-helper.el' adds quicklisp to Emacs load path

(defun dh/load-quicklisp-slime-helper ()
  "Perform the setup for Common LISP development with SLIME."
  (interactive)
  (setq-default inferior-lisp-program "sbcl")
  (load (expand-file-name "~/quicklisp/slime-helper.el")))

(add-hook 'lisp-mode-hook #'dh/load-quicklisp-slime-helper)


;; -----------------------------------------------------------------------------
;; Writing tools (Note-taking and Markdown documents)

;; Line wrapping for text modes
(use-package visual-line-mode
  :ensure nil
  :diminish visual-line-mode
  :hook (text-mode))

;; On-the-fly spell checker
(use-package flyspell
  :ensure nil
  :hook (text-mode)
  :bind* (:map flyspell-mouse-map
	       ("<mouse-3>" . 'flyspell-correct-word)))

;; Spell checking tool
(use-package ispell
  :after (flyspell)
  :ensure nil
  :custom
  (ispell-personal-dictionary "~/.hunspell")
  (ispell-cmd-args "-p ~/.hunspell")
  (ispell-program-name "hunspell"))

;; Citation manager (integrates with Zotero via better-bibtex)
(use-package citar
  :defer nil
  :after org-roam
  :init
  (define-prefix-command 'dh/citar-map)
  :custom
  (citar-bibliography '("~/Zotero/references.bib"))
  (org-cite-global-bibliography '("~/Zotero/references.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :bind
  ("C-c b" . dh/citar-map)
  (:map dh/citar-map
	("b" . citar-open)
	("i" . citar-insert-citation))
  :hook
  (org-mode . citar-capf-setup)
  (markdown-mode . citar-capf-setup))

;; Edit markdown documents
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :custom
  (markdown-command "pandoc"))

;; Edit Rmarkdwon documents
(use-package poly-R
  :init (require 'ess)
  :after (ess markdown)
  :mode ("\\.[rR]md\\'" . poly-gfm+r-mode)
  :custom
  (markdown-code-block-braces t)
  (markdown-asymmetric-header t))


;; -----------------------------------------------------------------------------
;; Org

(defvar org-directory (expand-file-name "~/org"))

(use-package org
  :defer nil
  :custom
  ;; File locations and refiling
  (org-agenda-files (list org-directory))
  (org-archive-location
   (concat (file-name-as-directory org-directory) "archive/archive23.org::"))
  (org-refile-targets
	'((nil :maxlevel . 9)
          (org-agenda-files :maxlevel . 9)))
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path t)

  ;; Capture
  (org-capture-templates '(("t" "Task" entry
			    (file "inbox.org")
			    "* TODO %i%?")))

  ;; Agenda
  (org-agenda-remove-tags t)
  (org-agenda-custom-commands '(("u" "School" tags-todo "ubc")
				("r" "Raincoast" tags-todo "raincoast")
				("p" "Personal" tags-todo "personal")))

  (holiday-local-holidays		; Define local holidays
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

  (holiday-general-holidays nil)	; Remove US and Religeous holidays
  (holiday-christian-holidays nil)
  (holiday-islamic-holidays nil)
  (holiday-bahai-holidays nil)
  (holiday-hebrew-holidays nil)
  (holiday-oriental-holidays nil)
  (calendar-holidays holiday-local-holidays)

  ;; Behaviour of RET in org buffers
  (org-return-follows-link t)		; press enter to follow links
  (fill-prefix "") ; fixes some bug with pressing RET for newlines

  :config
  ;; save all org-agenda buffers upon exit
  (advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)

  :bind
  ("C-c a" . 'org-agenda)
  ("C-c c" . 'org-capture))

(use-package org-roam
  :defer nil
  :init (define-prefix-command 'dh/org-roam-map)
  :custom
  (org-roam-directory "~/org/roam/")
  :bind
  ("C-c n" . dh/org-roam-map)
  (:map dh/org-roam-map
	("b" . org-roam-buffer-toggle)
        ("f" . org-roam-node-find)
        ("g" . org-roam-graph)
        ("i" . org-roam-node-insert)
        ("n" . org-roam-capture)
        ("j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode))

(use-package citar-org-roam
  :defer nil
  :after (citar org-roam)
  :no-require
  :diminish citar-org-roam-mode
  :custom (citar-org-roam-subdir "bib")
  :config (citar-org-roam-mode))


;; -----------------------------------------------------------------------------
;; Reading, News, and Multimedia

;; PDF reader
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook
  ('pdf-view-mode-hook  . 'pdf-view-themed-minor-mode))

;; EPUB reader
(use-package nov
  :mode
  (("\\.epub\\'" . nov-mode)))

;; E-book library management
(use-package calibre
  :custom
  (calibre-libraries
   '(("Fiction" . "~/Documents/Library/Calibre/Fiction")
     ("Non-Fiction" . "~/Documents/Library/Calibre/Non-Fiction"))))

;; News feeds
(use-package elfeed
  :bind
  ("C-c e" . 'elfeed)
  :custom
  (elfeed-search-filter "@1-week-ago +unread ")
  :config
  (use-package elfeed-org
    :config
    (elfeed-org))
  (use-package elfeed-tube
    :bind (:map elfeed-search-mode-map
		("F" . 'elfeed-tube-fetch)))
  (use-package elfeed-tube-mpv
    :bind (:map elfeed-show-mode-map
		("Y" . 'elfeed-tube-mpv))))

;; Internet Radio
(use-package eradio
  :init (define-prefix-command 'dh/eradio-map)
  :ensure nil
  :bind
  ("C-c r" . dh/eradio-map)
  (:map dh/eradio-map
	("p" . eradio-play)
	("s" . eradio-stop)
	("t" . eradio-toggle))
  :custom
  (eradio-player '("mpv" "--no-video" "--no-terminal"))
  (eradio-channels
   '(("CBC Van" . "http://playerservices.streamtheworld.com/pls/CBU2FM_CBC.pls")
     ("CBC Vic" . "http://playerservices.streamtheworld.com/pls/CBCVFM_CBC.pls")
     ("CBC Music" . "http://playerservices.streamtheworld.com/pls/CBUFM_CBC.pls")
     ("AM730" . "https://corus.leanstream.co/CKGOAM-MP3")
     ("CFUV" . "https://ais-sa1.streamon.fm/7132_64k.aac")
     ("CiTR" . "https://live.citr.ca/live.aac")
     ("Afro" . "https://play.streamafrica.net/afrofusion")
     ("Lofi" . "https://play.streamafrica.net/lofiradio")
     ("Reggae" . "https://play.streamafrica.net/reggaerepublic")
     ("Groove" . "https://somafm.com/groovesalad256.pls")
     ("Groove Classic" . "https://somafm.com/gsclassic.pls")
     ("World" . "https://somafm.com/suburbsofgoa.pls")
     ("Indie" . "https://somafm.com/indiepop.pls"))))

;; Watch YouTube videos
(use-package yeetube
  :custom
  (yeetube-default-sort-column "Date")
  (yeetube-display-thumbnails nil)
  :bind
  ("C-c y" . 'yeetube-search))


;; -----------------------------------------------------------------------------
;; Start-up profiling

;; See the start-up timer in `early-init.el'

;; ;; Profile start-up time on a per-package basis
;; (use-package esup
;;     :custom
;;     (esup-depth 0))

;; The garbage collection threshold is set very low in early init to reduce
;; start-up time. Here we return it to a reasonable value.
(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here
