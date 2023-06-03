;; init.el

;; Commentary: This file initializes my package management, then sources my org-based init file for custom configurations.

(load "~/.emacs.d/lisp/straight-bootstrap.el" nil t)

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

(require 'org)
(if (eq system-type 'windows-nt)
	(org-babel-load-file '"C:/Users/dhenn/.emacs.d/emacs-init.org")
	(org-babel-load-file '"/home/dhenn/.emacs.d/emacs-init.org"))
