(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)
(setq inhibit-splash-screen t)
(setq custom-safe-themes t)

(if (string-equal system-type "windows-nt")
    (add-to-list 'default-frame-alist '(font . "Cascadia Code 10" ))
    (add-to-list 'default-frame-alist '(font . "IBM Plex Mono 11" )))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'ess)
(defun my-inferior-ess-init ()
  (setq-local ansi-color-for-comint-mode 'filter))
(defun my-ess-init ()
  (setq-local ess-use-flymake nil))
(add-hook 'ess-mode-hook 'my-ess-init)
(add-hook 'inferior-ess-mode-hook 'my-inferior-ess-init)

(straight-use-package 'poly-R)

(straight-use-package 'flycheck)
(setq flycheck-lintr-linters 
      "linters_with_defaults(trailing_blank_lines_linter = NULL)")
(global-flycheck-mode)

(straight-use-package 'company)
(global-company-mode)

(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(straight-use-package 'evil)
(evil-mode)

(straight-use-package 'doom-themes)
(load-theme 'doom-material-dark)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
