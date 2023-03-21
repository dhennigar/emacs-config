;; r-programming.el

;; emacs speaks statistics and rmarkdown abilities
(use-package ess)
(defun my-inferior-ess-init ()
  (setq-local ansi-color-for-comint-mode 'filter))
(defun my-ess-init ()
  (setq-local ess-use-flymake nil)
  (flycheck-mode))
(add-hook 'ess-mode-hook 'my-ess-init)
(add-hook 'inferior-ess-mode-hook 'my-inferior-ess-init)

;; support for Rmd with code execution and knitting/sweaving.
(use-package poly-R)
