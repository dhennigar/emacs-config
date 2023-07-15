;; early-init.el

(setq package-enable-at-startup nil)

(add-to-list 'default-frame-alist '(font . "IBM Plex Mono-12" ))
(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(fringe-mode 0)

(setq custom-safe-themes t)

(add-hook 'window-configuration-change-hook
          (lambda ()
            (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 1 1)))

(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)

;; backups are stored in the system temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
