;;; init.el --- My Emacs configuration file

;; Copyright (c) 2022-2024 Daniel Hennigar

;; Author: Daniel Hennigar
;; URL: https://github.com/dhennigar/emacs-config
;; Package-Requires: ((emacs "29.4"))

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

;; Straight.el --- Functional package management using Git repositories
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Integration with the `use-package' macro
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)



;; -----------------------------------------------------------------------------
;; Aesthetics

(defun load-theme-disable-others (theme)
  "Enable THEME while disabling all other themes."
  (interactive
   (list
    (intern (completing-read
	     "Load custom theme: "
             (mapcar #'symbol-name
		     (custom-available-themes))))))
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme theme)
  (message (concat "Changed theme to " (symbol-name theme))))

;; Toggle line highlighting and numbering
(bind-key "C-c h" #'hl-line-mode)
(bind-key "C-c l" #'display-line-numbers-mode)

;; My tomorrow themes
(use-package drh-tomorrow
  :straight nil
  :ensure nil)

;; My monochromatic themes
(use-package drh-mono
  :straight nil
  :ensure nil)

;; Minimalist mode line
(use-package mood-line
  :config
  (mood-line-mode))

;; Hide minor modes from the mode line
(use-package diminish)			; consider replacing with blackout.el


;; -----------------------------------------------------------------------------
;; File Management

;; The built-in file manager
(use-package dired
  :straight nil
  :ensure nil
  :config
  ;; Icons for dired
  (use-package all-the-icons
    :ensure t
    :if (display-graphic-p))
  (use-package all-the-icons-dired
    :ensure t
    :diminish)
  ;; Regex for excluding hidden files
  (setq dired-omit-files
	(rx (or (seq bol (? ".") "#")
		(seq bol "." (not (any ".")))
		(seq "~" eol)
		(seq bol "CVS" eol))))
  ;; Remove dired buffers after we're done with them
  (setq dired-kill-when-opening-new-dired-buffer t)
  ;; Hooks
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (defun dired-toggle-details-and-icons ()
    "Toggle `dired-hide-details-mode' and `all-the-icons-dired-mode'."
    (interactive)
    (dired-hide-details-mode 'toggle)
    (all-the-icons-dired-mode 'toggle))
  :bind
  ("C-x C-d" . 'dired)		     ; since `list-directory' is trash
  (:map dired-mode-map
	("h" . 'dired-omit-mode)	   ; toggle hidden files
	("H" . 'dired-toggle-details-and-icons))) ; toggle detailed view


;; -----------------------------------------------------------------------------
;; Navigation

;; Switch windows quickly
(bind-key* "C-x <right>" 'windmove-right)
(bind-key* "C-x <left>"  'windmove-left)
(bind-key* "C-x <up>"    'windmove-up)
(bind-key* "C-x <down>"  'windmove-down)

;; Move buffers around
(use-package buffer-move
  :bind
  (("C-x S-<right>" . 'buf-move-right)
   ("C-x S-<left>" . 'buf-move-left)
   ("C-x S-<up>" . 'buf-move-up)
   ("C-x S-<down>" . 'buf-move-down)))

;; Re-assign some unnatural keybindings
(bind-key "C-x C-b" 'switch-to-buffer)	 ; since buffer-list is trash
(bind-key "C-M-g" 'exit-recursive-edit)  ; a more natural exit command
(unbind-key "C-?")                       ; formerly undo-redo
(bind-key "C-x M-u" 'undo-redo)          ; remapping undo-redo
(bind-key "M-]" 'forward-paragraph)	 ; nicer than "M-}"
(bind-key "M-[" 'backward-paragraph)	 ; nicer than "M-{"
(bind-key "M-<down>" 'forward-paragraph) ; nicer than "M-}"
(bind-key "M-<up>" 'backward-paragraph)	 ; nicer than "M-{"

;; Superior search
(use-package ctrlf
  :config
  (ctrlf-mode))

;; Superior substitue
(use-package substitute
  :bind
  ("C-c s" . 'substitute-prefix-map))

;; Show possible key chord completions
(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 5)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode +1))

;; Select the *Help* buffer when created
(setq help-window-select t)


;; -----------------------------------------------------------------------------
;; Autocompletion

;; See `drh-completion' for autocompletion using built-in functionality only.
;; Use in case of broken packages below. Servicable, but not as ergonomic.
;; (require 'drh-completion)

(add-hook 'prog-mode-hook 'electric-pair-mode) ; Match delimiters for coding
;; (setq tab-always-indent 'complete)	       ; Context aware tab completion
;; (setq tab-first-completion nil)	       ; Offer choice of completions

;; Out of order search term filtering
(use-package orderless
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides
	'((file (styles basic partial-completion)))))

;; Useful details in the minibuffer
(use-package marginalia
  :config (marginalia-mode))

;; Vertically list completion candidates
(use-package vertico
  :config
  (setq vertico-count 8)
  (setq vertico-resize nil)
  (vertico-mode))

;; Remember previous choices (combines well with vertico)
(use-package savehist
  :config
  (savehist-mode))

;; Completion-at-point overlay
(use-package corfu
  :config

  (setq corfu-auto t)
  (setq corfu-quit-no-match t)

  (setq corfu-left-margin-width 0)
  (setq corfu-right-margin-width 0)
  
  (require 'corfu-popupinfo)
  (setq corfu-popupinfo-delay nil)  
  (corfu-popupinfo-mode)
  
  (global-corfu-mode +1)

  :bind
  (:map corfu-map
	("M-SPC" . 'corfu-insert-separator)
	("?" . 'corfu-popupinfo-toggle)))

;; Expand abbreviations for quick typing
(use-package abbrev
  :straight nil
  :ensure nil
  :diminish abbrev-mode
  :config
  (setq abbrev-file-name "~/.emacs.d/abbrevs")  
  ;; this tells abbrev mode to look for words which start with "/"
  (abbrev-table-put global-abbrev-table
		    :regexp "\\(?:^\\|[\t\s]+\\)\\(?1:[/].*\\)")
  :hook
  (prog-mode . abbrev-mode)
  (text-mode . abbrev-mode))


;; -----------------------------------------------------------------------------
;; General programming tools

;; Documentation on hover
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :config
  ;; A small delay in Eldoc prevents it from stepping on my typing.
  (setq eldoc-idle-delay 1.0))

;; I don't like the documentation-on-hover provided by most LSP servers.
(add-hook 'eglot-managed-mode-hook (lambda () (eldoc-mode -1)))

;; Real-time syntax checking
(use-package flymake
  :ensure nil
  :init (define-prefix-command 'dh/flymake-map)
  :config
  (setq flymake-no-changes-timeout 2.0)
  :bind
  (:map global-map
	("C-c f" . dh/flymake-map))
  (:map dh/flymake-map
	("f" . flymake-mode)
	("n" . flymake-goto-next-error)
	("p" . flymake-goto-prev-error)
	("b" . flymake-show-buffer-diagnostics)))

;; Install treesitter grammars
(setq treesit-language-source-alist
      '((markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python   "https://github.com/tree-sitter/tree-sitter-python")
	(bash     "https://github.com/tree-sitter/tree-sitter-bash")
	(elisp    "https://github.com/Wilfred/tree-sitter-elisp")
	(r        "https://github.com/r-lib/tree-sitter-r")
	(perl '("https://github.com/tree-sitter-perl/tree-sitter-perl" "release"))
	(pod  '("https://github.com/trees-sitter-perl/tree-sitter-pod" "release"))))


;; -----------------------------------------------------------------------------
;; Integrated terminal emulator

(use-package vterm
  :bind
  ("C-c t" . 'vterm-other-window)
  ("C-c M-t" . 'vterm)
  (:map vterm-mode-map
	("C-q" . 'vterm-send-next-key)))


;; -----------------------------------------------------------------------------
;; Bash

;; Use LSP for bash scripts
;; note: must install `'bash-language-server', `shfmt', and `shellcheck'
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
	       '((sh-mode bash-ts-mode) . ("bash-language-server" "start"))))
(add-hook 'sh-mode-hook 'eglot-ensure)
(add-hook 'bash-ts-mode-hook 'eglot-ensure)

;; and use treesitter mode by default
(add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))


;; -----------------------------------------------------------------------------
;; R

;; Emacs Speaks Statistics
(use-package ess
  :config
  (setq ess-default-style 'RStudio)
  (setq ess-use-flymake t)
  (setq ess-r-flymake-linters ; arguments passed to lintr::default_linters()
	'("trailing_blank_lines_linter = NULL"
	  "indentation_linter = indentation_linter(indent = 2L)"))
  (setq inferior-R-args "--no-save")
  (setq ess-R-font-lock-keywords
	'((ess-R-fl-keyword:modifiers . t)
	  (ess-R-fl-keyword:fun-defs)
	  (ess-R-fl-keyword:keywords . t)
	  (ess-R-fl-keyword:assign-ops)
	  (ess-R-fl-keyword:constants . t)
	  (ess-fl-keyword:fun-calls . t)
	  (ess-fl-keyword:numbers)
	  (ess-fl-keyword:operators)
	  (ess-fl-keyword:delimiters)
	  (ess-fl-keyword:=)
	  (ess-R-fl-keyword:F&T)
	  (ess-R-fl-keyword:%op%)))
  (require 'ess-r-mode)
  (defun dh/ess-insert-pipe ()
    "Insert `%>%', trim whitespace, and insert newline with indentation."
    (interactive)
    (unless (ess-inside-string-or-comment-p)
      (delete-horizontal-space)
      (insert " %>% ")
      (ess-roxy-newline)))
  (bind-key "<f1>" 'ess-display-help-on-object ess-r-mode-map)
  (bind-key "C-," 'ess-insert-assign ess-r-mode-map)  ; insert `<-'
  (bind-key "C-." 'dh/ess-insert-pipe ess-r-mode-map) ; insert `%>%'
  (bind-key "C-c C-r" 'R)			      ; start R
  (bind-key "C-c C-r" 'R ess-r-mode-map))


;; -----------------------------------------------------------------------------
;; Python

;; A complete IDE for Python in Emacs
(use-package elpy
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq python-shell-interpreter "jupyter") ; an improved Python REPL experience
  (setq python-shell-interpreter-args "console --simple-prompt")
  (setq python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter"))


;; -----------------------------------------------------------------------------
;; Common LISP

(defun dh/load-quicklisp-slime-helper ()
  "Perform the setup for Common LISP development with SLIME."
  (interactive)
  (setq-default inferior-lisp-program "sbcli")
  (load (expand-file-name "~/quicklisp/slime-helper.el")))

(add-hook 'lisp-mode-hook #'dh/load-quicklisp-slime-helper)


;; -----------------------------------------------------------------------------
;; Perl

;; use the more modern Perl mode by default
(add-to-list 'major-mode-remap-alist '(perl-mode . cperl-mode))
(setq cperl-invalid-face nil)
(setq cperl-indent-parens-as-block t)
(setq cperl-indent-level 4)
(setq cperl-close-paren-offset (- cperl-indent-level))

;; use PerlNavigator LSP via eglot
(setq-default eglot-workspace-configuration
              '((:perlnavigator . (:perlPath
				   "/usr/bin/perl"
				   :enableWarnings t))))
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `((cperl-mode perl-mode) . ("~/perl5/bin/perlnavigator", "--stdio"))))
(add-hook 'cperl-mode-hook 'eglot-ensure)
(add-hook 'perl-mode-hook 'eglot-ensure)

;; tidy up your code
(use-package perltidy
  :straight (perltidy
	     :type git
	     :host github
	     :repo "perl-ide/perltidy.el"
	     :branch "master"))


;; -----------------------------------------------------------------------------
;; For publishing software
(use-package legalese
  :config
  (setq legalese-default-author "Daniel Hennigar")
  (setq legalese-default-copyright "Daniel Hennigar")
  (add-to-list
   'legalese-licenses
   ;; Use as licensing boilerplate for Perl programs
   '(dual "This program is free software; you can redistribute it and/or modify
it under the terms of either:

  a) the Artistic License 2.0, or
  b) the GNU General Public License as published by the Free Software
     Foundation; either version 3, or (at your option) any later version.

See the LICENSE file for more information.")))


;; -----------------------------------------------------------------------------
;; Writing Documents

;; Zettelkasten
(use-package denote
  :init
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 '("n" "New note (with Denote)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t)))
  :config
  (setq denote-directory (expand-file-name "~/org/notes/"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-date-prompt-use-org-read-date t)
  (setq denote-org-store-link-to-heading nil)
  ;; Use denote for journaling
  (require 'denote-journal-extras)
  (setq denote-journal-extras-direcotry "~/org/journal/")
  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
		 '("j" "Journal" entry
                   (file denote-journal-extras-path-to-new-or-existing-entry)
                   "* %U %?\n%i\n%a"
                   :kill-buffer t
                   :empty-lines 1)))
  ;; Take bibliographic notes with denote
  (use-package citar-denote
    :after denote
    :init (citar-denote-mode 1)
    :config
    (setq citar-denote-subdir "bib"))
  :bind
  ("C-c n" . 'denote-open-or-create))

;; Manage citations and view their attachments
(use-package citar
  :config
  (setq citar-bibliography '("~/Zotero/references.bib"))
  (setq org-cite-global-bibliography '("~/Zotero/references.bib"))
  (setq org-cite-insert-processor 'citar)
  (setq org-cite-follow-processor 'citar)
  (setq org-cite-activate-processor 'citar)
  :bind
  ("C-c b" . 'citar-open)
  :hook
  (org-mode . citar-capf-setup)
  (markdown-mode . citar-capf-setup))

;; Settings for writing Rmarkdown files
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :config
  (setq markdown-command "pandoc"))
(use-package poly-R
  :after (ess markdown)
  :mode ("\\.[rR]md\\'" . poly-gfm+r-mode)
  :config
  (setq markdown-code-block-braces t)
  (setq markdown-asymmetric-header t))

;; Line wrapping for text modes
(add-hook 'text-mode-hook #'visual-line-mode)
(diminish 'visual-line-mode)


;; -----------------------------------------------------------------------------
;; Spell checking and word lookup

;; On-the-fly spell checker
(add-hook 'text-mode-hook #'flyspell-mode)
;;(bind-key "<mouse-3>" 'flyspell-correct-word flyspell-mode-map)

;; Spell checking back end
(setq ispell-personal-dictionary "~/.hunspell")
(setq ispell-cmd-args "-p ~/.hunspell -d en_CA")
(setq ispell-program-name "hunspell")
(unless (file-exists-p ispell-personal-dictionary)
  (write-region "" nil ispell-personal-dictionary nil 0))

;; Quickly look up words
(setq dictionary-server "localhost")
(setq dictionary-default-dictionary "wn")
(setq dictionary-create-buttons nil)


;; -----------------------------------------------------------------------------
;; Org Mode

(use-package org
  :straight nil
  :diminish org-indent-mode
  :config
  ;; File paths
  (setq org-directory "~/org")
  (setq org-agenda-files (list "~/org" "~/org/class"))
  (setq org-archive-location "~/org/archive/archive24.org::")
  ;; Refiling
  (setq org-refile-targets
	'((nil :maxlevel . 9)
          (org-agenda-files :maxlevel . 9)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)
  
  ;; No automatic bookmarks
  (setq org-capture-bookmark nil)
  (setq org-bookmark-names-plist nil)
  
  ;; Navigation
  (setq org-return-follows-link t)	; press enter to follow links
  
  ;; Org-capture settings
  ;; (setq org-id-link-to-org-use-id 'create-if-interactive)
  (setq org-capture-templates '(("t" "Task" entry
				 (file+headline "planner.org" "Inbox")
				 "* TODO %?")
				("m" "Task (Masters)" entry
				 (file+headline "thesis.org" "Inbox")
				 "* TODO %?")
				("d" "Daily" entry
				 (file+headline "planner.org" "Daily")
				 "* TODO %?")))

  ;; Org-agenda settings
  (require 'org-agenda-holidays)
  (setq org-agenda-remove-tags t)
  (setq org-agenda-custom-commands '(("u" "School" tags-todo "ubc")
				     ("r" "Raincoast" tags-todo "rcf")
				     ("p" "Personal" tags-todo "per")
				     ("d" "Daily" tags-todo "daily")))
  
  ;; Save all org-agenda buffers upon exit
  (advice-add 'org-agenda-quit :before #'org-save-all-org-buffers)
  
  ;; Aesthetics
  (setq org-startup-indented t)	     ; indent buffer by heading level
  (setq org-hide-emphasis-markers t) ; hide the markup on bold/italics
  (setq org-pretty-entities t)	     ; use UTF-8 characters
  (font-lock-add-keywords	     ; replace "-" with "•"
   'org-mode
   '(("^ *\\([-]\\) "
      (0 (prog1 ()
	   (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
  (use-package mixed-pitch
    :config
    (add-hook 'org-mode-hook (lambda () (mixed-pitch-mode 1))))
  
  ;; Dynamically hide emphasis markers based on cursor position
  (use-package org-appear
    :after org
    :config
    ;; These are paired with the settings under `Aesthetics' above
    (setq org-appear-autoemphasis t)
    (setq org-appear-autolinks t)
    (setq org-appear-autosubmarkers t)
    (setq org-appear-autoentities t)
    (add-hook 'org-mode-hook #'org-appear-mode))
  
  :bind
  ("C-c a" . 'org-agenda)
  ("C-c c" . 'org-capture))


;; -----------------------------------------------------------------------------
;; Reading Documents

(use-package calibre
  :config
  (setq calibre-libraries
	'(("Calibre" . "~/Documents/Library/Calibre/")))
  (setq calibre-library-columns
	'((title . 40)
	  (authors . 40)
	  (tags . 10))))

(use-package nov
  :mode (("\\.epub\\'" . nov-mode)))

(use-package pdf-tools
  :mode (("\\.pdf\\'" . pdf-view-mode)))


;; -----------------------------------------------------------------------------
;; Study timer

;; Track work and break time according to the `Third Time' system
;; as described on <https://www.lesswrong.com>
(use-package pomm
  :init
  (define-prefix-command 'dh/pomm-map)
  :config
  (setq pomm-audio-enabled t)
  (setq pomm-audio-player-executable "mpv")
  (setq pomm-third-time-csv-history-file "~/.emacs.d/pomm.csv")
  (pomm-mode-line-mode)  
  :bind
  ("C-c p" . 'dh/pomm-map)
  (:map dh/pomm-map
	("s" . 'pomm-third-time-start)
	("S" . 'pomm-third-time-stop)
	("b" . 'pomm-third-time-switch)
	("r" . 'pomm-third-time-reset)))


;; -----------------------------------------------------------------------------
;; Consuming Media

(use-package elfeed
  :init
  (run-with-timer 0 (* 60 15) 'elfeed-update)
  :config
  (use-package elfeed-org
    :after elfeed
    :init
    (elfeed-org))
  (defun elfeed-show-play-enclosure-empv (enclosure-index)
    "Play elfeed media entries by sending their ENCLOSURE-INDEX to empv."
    (interactive (list (elfeed--enclosure-maybe-prompt-index elfeed-show-entry)))
    (let ((url (car
		(elt
		 (elfeed-entry-enclosures elfeed-show-entry)
		 (- enclosure-index 1)))))
      (empv-play url)))
  (setq elfeed-search-filter "@1-month-ago +unread")
  :bind
  ("C-c e" . 'elfeed)
  (:map elfeed-show-mode-map
	("P" . 'elfeed-show-play-enclosure-empv)))

(use-package empv
  :init
  (define-prefix-command 'dh/empv-map)
  :config
  (add-to-list 'empv-mpv-args "--ytdl-format=best")
  (setq empv-base-directory "~/Music/")
  (setq empv-video-dir "~/Videos")
  (setq empv-invidious-instance "https://invidious.fdn.fr/api/v1")
  (setq empv-radio-channels
	'(("SomaFM - Groove Salad" . "http://www.somafm.com/groovesalad.pls")
	  ("SomaFM - Drone Zone" . "http://www.somafm.com/dronezone.pls")
	  ("SomaFM - Sonic Universe" . "https://somafm.com/sonicuniverse.pls")
	  ("SomaFM - Vaporwaves" . "https://somafm.com/vaporwaves.pls")
	  ("CBC - Vancouver" .
 	   "http://playerservices.streamtheworld.com/pls/CBU2FM_CBC.pls")))
  :bind
  ("C-c m" . 'dh/empv-map)
  (:map dh/empv-map
	;; Choose media to play
	("m" . 'empv-play-directory)	; Play an album
	("M" . 'empv-play-audio)	; Play a single track
	("v" . 'empv-play-video)
	("y" . 'empv-youtube)
	("r" . 'empv-play-radio)
	;; Playback controls
	("p" . 'empv-toggle)		; Pause and unpause
	("n" . 'empv-playlist-next)
	("N" . 'empv-playlist-prev)
	("s" . 'empv-playlist-shuffle)
	("q" . 'empv-exit)))


;; -----------------------------------------------------------------------------
;; Browse the Web, Geminispace, or Gopherspace

;; Built in browsers
(define-prefix-command 'dh/browser-map)
(bind-key "C-c w" 'dh/browser-map)
(bind-key "w" 'eww 'dh/browser-map)	; mnemonic: "web"
(use-package elpher
  :bind
  (:map dh/browser-map ("g" . elpher-go))) ; mnemonic: "gopher"

;; Duckduckgo search the thing at point
(defun ddg-current-word ()
  ;; initially written by chatgpt but later modified by u/Aminumbra
  "Search the current word on Google using browse-url."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (if word
        (browse-url (concat "https://www.duckduckgo.com/" word))
      (message "No word found at point."))))

(bind-key "C-c g" #'ddg-current-word) 	; mnemonic: "google"

;; Used to serve my simple static website locally
;; See the `site' subdirectory in my org files.
(use-package simple-httpd)


;; -----------------------------------------------------------------------------
;; Start-up profiling

;; See the start-up timer in `early-init.el' which indicates total start-up
;; time in a message to the user.

;; ;; Measures start-up time for each function in your init file. Call `M-x esup'
;; (use-package esup
;;   :defer t
;;   :config
;;   (setq esup-depth 0))


;; -----------------------------------------------------------------------------
;; Optimization

(require 'midnight)
(midnight-delay-set 'midnight-delay 60)

;; The garbage collection threshold is set very low in early init to reduce
;; start-up time. Here we return it to a reasonable value.
(setq gc-cons-threshold (* 2 1000 1000))


;;; init.el ends here
