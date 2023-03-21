;; early-init.el

(setq package-enable-at-startup nil)

;; default font settings
(add-to-list 'default-frame-alist '(font . "IBM Plex Mono-12" ))

;; basic gui settings
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(setq custom-safe-themes t)

;; startup options
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)

;; backups are stored in the system temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
