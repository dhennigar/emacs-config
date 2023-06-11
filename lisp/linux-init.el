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

;;(use-package sudo-edit)
