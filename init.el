;;; init.el --- Daniel's init file
;;; Commentary:
;;   Uses minimal external packages for R development.

;;; Code:

;; package repositories
(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; use-package initialization
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;; use-package declarations
(use-package ess
  :ensure t
  :init (require 'ess-site)
  :config (setq ess-use-flymake nil))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode t)
  :custom (flycheck-lintr-linters
            "linters_with_defaults(trailing_blank_lines_linter = NULL)"))

(use-package company
  :init (global-company-mode t))

(use-package nimbus-theme
  :init (load-theme 'nimbus t))

;; midnight mode
(require 'midnight)
(midnight-delay-set 'midnight-delay "4:30am")

;; set font on windows
(if (string-equal system-type "windows-nt")
    (add-to-list 'default-frame-alist '(font . "Cascadia Code 10" )))

;; general settings
(setq auto-save-default nil)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)

(provide 'init)
;;; init.el ends here

