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

(use-package diminish)


;; Theme settings -----------------------------------------------

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

;(defvar current-hour
;  (nth 2 (decode-time (current-time))))

(if (and (< (nth 2 (decode-time (current-time))) 20)
	 (>= (nth 2 (decode-time (current-time))) 6))
    (load-theme 'modus-operandi)
  (load-theme 'modus-vivendi))

;; OS-Specific Configuration ------------------------------------

;; (if (eq system-type 'gnu/linux)
    ;; any linux-specific config goes here
;;     )

(if (eq system-type 'windows-nt)
    (setq ring-bell-function 'ignore))


;; Autocompletion -----------------------------------------------

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

(use-package vertico
  :init (vertico-mode))

(use-package corfu
  :custom
  (corfu-auto nil)
  (corfu-quit-no-match t)
  :bind
  (:map corfu-map ("SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode)
  :hook
  ('eshell-mode-hook . (lambda ()
			 (setq-local corfu-auto nil)
			 (corfu-mode))))

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


;; Org-mode --------------------------------------------------------

(use-package org
  :ensure nil
  :custom
  (org-indent-indentation-per-level t)
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-todo-keywords
   '((sequence
      "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-agenda-files '("~/Documents/Org/raincoast.org"
			   "~/Documents/Org/masters.org"
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
				 "* %i%?")))
  (holiday-local-holidays
	'(
	  (holiday-fixed 1 1 "New Year's Day") ; 1st of January
	  (holiday-float 2 1 3 "Family Day") ; third Monday in February
	  (holiday-easter-etc -2 "Good Friday") ; it's complicated...
	  (holiday-float 5 1 -2 "Victoria Day") ; Monday preceding 25th of May
	  (holiday-fixed 6 21 "Indigenous Peoples Day") ; 21st of June
	  (holiday-fixed 7 1 "Canada Day") ; 1st of July
	  (holiday-float 8 1 1 "BC Day") ; first Monday in August
	  (holiday-fixed 9 30 "National Day for Truth and Reconcilliation") ; 30th of September
	  (holiday-float 10 1 2 "Canadian Thanksgiving") ; second Monday in October
	  (holiday-fixed 10 31 "Halloween") ; 31st of October
	  (holiday-fixed 11 11 "Rememberance Day") ; 11th of November
	  (holiday-float 11 4 4 "American Thanksgiving") ; fourth Thursday of November
	  (holiday-fixed 12 25 "Christmas") ; 25th of December
	  ))
  (holiday-general-holidays nil)
  (holiday-christian-holidays nil)
  (holiday-islamic-holidays nil)
  (holiday-bahai-holidays nil)
  (holiday-hebrew-holidays nil)
  (holiday-oriental-holidays nil)

  (calendar-holidays holiday-local-holidays)

  (org-confirm-babel-evaluate nil)
  
  :hook
  ('org-mode-hook #'visual-line-mode)
  ('org-mode-hook #'auto-fill-mode)

  :bind
  ("C-c a" . 'org-agenda)
  ("C-c c" . 'org-capture))
  
(use-package ob-R
  :ensure nil
  :defer t
  :commands (org-babel-execute:R))

(use-package ob-lisp
  :ensure nil
  :defer t
  :commands (org-babel-execute:lisp))

;; Programming --------------------------------------------------

;; Flycheck
(use-package flycheck
  :init
  (global-flycheck-mode t)
  :custom
  (flycheck-lintr-linters
   "linters_with_defaults(trailing_blank_lines_linter = NULL)")
  (flycheck-check-syntax-automatically '(save mode-enabled))
  :bind
  ("C-c f f" . 'flycheck-buffer)
  ("C-c f n" . 'flycheck-next-error)
  ("C-c f p" . 'flycheck-previous-error))

;; Eldoc
(use-package eldoc
  :defer 2
  :custom
  (eldoc-idle-delay 3)
  :bind
  ("C-c e" . eldoc))

;; Language-specific configs

(use-package ess
  :custom
  (ess-use-flymake nil)
  (ess-use-eldoc nil)
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
     (ess-fl-keywor~operators . t)
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

(defun dh/load-slime-helper ()
  "Perform the setup for Common Lisp development with SLIME."
  (interactive)
  (setq-default inferior-lisp-program "sbcl")
  (load (expand-file-name "~/quicklisp/slime-helper.el")))

(use-package go-mode
  :defer 2)
(use-package go-complete
  :defer 2
  :hook
  ('completion-at-point-functions 'go-complete-at-point))

(use-package ahk-mode
  :defer 2)


;; Writing  ---------------------------------------------------

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
  ("C-c C-d" . denote)
  ("C-c d d" . denote)
  ("C-c d t" . denote-type)
  ("C-c d l" . denote-link)
  ("C-c d L" . denote-add-links)
  ("C-c d b" . denote-backlinks)
  ("C-c d f" . denote-find-link)
  ("C-c d F" . denote-find-backlink)
  ("C-c d r" . denote-rename-file)
  ("C-c d R" . denote-rename-file-using-front-matter))

(use-package citar-denote
  :diminish
  :functions citar-denote-mode
  :init (citar-denote-mode)
  :custom
  (citar-notes-paths '("~/Documents/Org/notes"))
  (citar-denote-title-format "author-year")
  (citar-denote-subdir t)
  :bind
  ("C-c b b" . citar-create-note)
  ("C-c b o" . citar-denote-open-note)
  ("C-c b ." . citar-denote-dwim)
  ("C-c b k" . citar-denote-add-citekey)
  ("C-c b M-k" . citar-denote-remove-citekey)
  ("C-c b M-r" . citar-denote-open-reference-entry)
  ("C-c b r" . citar-denote-find-reference)
  ("C-c b C-f" . citar-denote-find-citation))

(use-package markdown-mode
  :defines markdown-command
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :custom
  (markdown-command "pandoc"))

(use-package poly-R
  :mode ("\\.[rR]md\\'" . poly-gfm+r-mode)					
  :custom
  (markdown-code-block-braces t))


;; PDF and EPUB -------------------------------------------------------

(use-package pdf-tools
  :defer 2
  :defines pdf-view-themed-minor-mode
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook
  ('pdf-view-mode-hook  . 'pdf-view-themed-minor-mode))

(use-package calibredb
  :defer 2
  :custom
  (calibredb-db-dir "~/Documents/Calibre/metadata.db")
  (calibredb-library-alist '("~/Documents/Calibre")))

(use-package nov
  :defer 2
  :mode
  (("\\.epub\\'" . nov-mode))
  :custom
  (nov-unzip-program "tar")
  (nov-unzip-args '("-xC" directory "-f" filename)))


;; Eshell -------------------------------------------------------------

;; This is necessary since clear command sends any current input
;; for some reason. Also allows clearing the eshell from any buffer.

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

(use-package eshell-toggle
  :bind
  ("C-c e" . eshell-toggle))

;; Init profiling -----------------------------------------------------

;; This is used for profiling the startup time of your init files.
;; The startup timer message in early-init.el as another tool that
;; is useful for this purpose.

(use-package esup
   :custom
   (esup-depth 0))


;; General Keybindings ------------------------------------------------

(bind-key "C-M-g" 'exit-recursive-edit) ; a more natural exit command

(unbind-key "C-?") ; formerly undo-redo
(bind-key "C-x M-u" 'undo-redo)

(unbind-key "C-M-w") ; since I use this to start my web browser
(unbind-key "C-M-e") ; since I use this to start an emacs client


;; set the garbage collection back to something reasonable
(setq gc-cons-threshold (* 2 1000 1000))


;;; init.el ends here
