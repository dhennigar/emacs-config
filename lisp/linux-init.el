;;; linux-init.el

;; File Paths
(defvar org-dir "~/Documents/Org/")
(defvar note-dir "~/Documents/Org/Notes")
(defvar cite-dir "~/Documents/Org/Bib")

;; Terminal Emulator
(use-package vterm)
(use-package vterm-toggle
  :bind (("M-RET" . 'vterm-toggle)
	 :map vterm-mode-map
	 ("M-RET". 'vterm-toggle)))

;; Common Lisp
(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")

;; Golang
(use-package go-mode)
(add-hook 'go-mode-hook 'eglot-ensure)

