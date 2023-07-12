;;; linux-init.el

;; File Paths
(defvar my-org-dir "~/Documents/Org/")
(defvar my-roam-dir "~/Documents/Org/roam")
(defvar my-task-dir "~/Documents/Org/tasks")
(defvar my-org-gcal-file "~/Documents/Org/calendar.org")

;; Terminal Emulator
(use-package vterm)
(use-package vterm-toggle
  :bind (("M-RET" . 'vterm-toggle)
	 :map vterm-mode-map
	 ("M-RET". 'vterm-toggle)))

;; Common Lisp

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")


;; Golang
(use-package go-mode)
(add-hook 'go-mode-hook 'eglot-ensure)

