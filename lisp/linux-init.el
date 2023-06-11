;;; linux-init.el

;; File Paths
(defvar documents-directory "~/Documents/")
(defvar orgfiles-directory "~/Documents/Org/")

;; Terminal Emulator
(use-package vterm)
(use-package vterm-toggle
  :bind (("M-RET" . 'vterm-toggle)
	 :map vterm-mode-map
	 ("M-RET". 'vterm-toggle)))

;; Sudo
(use-package sudo-edit)

;; Common Lisp
(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")

;; Golang
(use-package go-mode)
(add-hook 'go-mode-hook 'eglot-ensure)

