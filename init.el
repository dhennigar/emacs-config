;; init.el

(load "~/.emacs.d/lisp/straight-bootstrap.el" nil t)


;; Autocompletion

(electric-pair-mode)

(use-package company
  :config (global-company-mode))


;; Aesthetics

(use-package modus-themes
  :init (load-theme 'modus-operandi)
  :custom (modus-operandi-palette-overrides nil)
  :bind ("C-c t" . modus-themes-toggle))

; (use-package tree-sitter
;   :config (tree-sitter-hl-mode)
;   :requires (tree-sitter-langs)
;   )
;; I think this is built in to 29.


;; R Programming

(use-package ess)
(defun my-inferior-ess-init ()
  (setq-local ansi-color-for-comint-mode 'filter))
(defun my-ess-init ()
  (setq-local ess-use-flymake nil)
  (flycheck-mode))
(add-hook 'ess-mode-hook 'my-ess-init)
(add-hook 'inferior-ess-mode-hook 'my-inferior-ess-init)

(use-package poly-R)
(add-to-list 'auto-mode-alist
             '("\\.[rR]md\\'" . poly-gfm+r-mode))
(setq markdown-code-block-braces t)


;; Linter
(use-package flycheck
  :custom (flycheck-lintr-linters 
	   "linters_with_defaults(trailing_blank_lines_linter = NULL)"))

;; Git
(use-package magit
  :bind (("C-x g" . magit-status)
	 ("C-x C-g" . magit-status)))


;;; Note Taking System

(use-package denote
  :custom 
  (denote-directory "~/Documents/Notes")
  (denote-known-keywords '("fre" "emacs" "salmon" "plants"))
  (denote-file-type "text")
  :config
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
  )

(let ((map global-map))
  (define-key map (kbd "C-c n j") #'my-denote-journal) ; our custom command
  (define-key map (kbd "C-c n n") #'denote)
  (define-key map (kbd "C-c n N") #'denote-type)
  (define-key map (kbd "C-c n d") #'denote-date)
  (define-key map (kbd "C-c n z") #'denote-signature) ; "zettelkasten" mnemonic
  (define-key map (kbd "C-c n s") #'denote-subdirectory)
  (define-key map (kbd "C-c n t") #'denote-template)
  ;; If you intend to use Denote with a variety of file types, it is
  ;; easier to bind the link-related commands to the `global-map', as
  ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
  ;; `markdown-mode-map', and/or `text-mode-map'.
  (define-key map (kbd "C-c n i") #'denote-link) ; "insert" mnemonic
  (define-key map (kbd "C-c n I") #'denote-link-add-links)
  (define-key map (kbd "C-c n b") #'denote-link-backlinks)
  (define-key map (kbd "C-c n f f") #'denote-link-find-file)
  (define-key map (kbd "C-c n f b") #'denote-link-find-backlink)
  ;; Note that `denote-rename-file' can work from any context, not just
  ;; Dired bufffers.  That is why we bind it here to the `global-map'.
  (define-key map (kbd "C-c n r") #'denote-rename-file)
  (define-key map (kbd "C-c n R") #'denote-rename-file-using-front-matter))

(let ((map dired-mode-map))
  (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
  (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-marked-files)
  (define-key map (kbd "C-c C-d C-R") #'denote-dired-rename-marked-files-using-front-matter))
