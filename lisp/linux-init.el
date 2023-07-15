;;; linux-init.el

;; Terminal Emulator
(use-package vterm)
(use-package vterm-toggle
  :bind (("C-c v" . 'vterm-toggle)
	 :map vterm-mode-map
	 ("C-c v". 'vterm-toggle)))

;; System Monitor
(use-package symon
  :config (symon-mode))

;; Common Lisp

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

;; Golang
(use-package go-mode)
(add-hook 'go-mode-hook 'eglot-ensure)

