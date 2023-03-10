;; init.el --- Daniel's init file

(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)
(setq inhibit-splash-screen t)
(setq custom-safe-themes t)

(if (string-equal system-type "windows-nt")
    (add-to-list 'default-frame-alist '(font . "Cascadia Code 10" ))
    (add-to-list 'default-frame-alist '(font . "IBM Plex Mono 11" )))

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

(use-package ess
  :config (setq ess-use-flymake nil))

(defun my-inferior-ess-init ()
  (setq-local ansi-color-for-comint-mode 'filter))
(add-hook 'inferior-ess-mode-hook 'my-inferior-ess-init)

(use-package poly-R
  :ensure t)

(use-package flycheck
  :init (global-flycheck-mode t)
  :custom (flycheck-lintr-linters
            "linters_with_defaults(trailing_blank_lines_linter = NULL)"))

(use-package company
  :init (global-company-mode t))

(use-package tree-sitter
  :ensure t)
(use-package tree-sitter-langs
  :ensure t)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(use-package evil
  :init (evil-mode 1))

(use-package github-theme
  :ensure t)

(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-material-dark))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
