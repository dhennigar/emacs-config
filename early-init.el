;; early-init.el

(setq package-enable-at-startup nil)

;; default font settings
(add-to-list 'default-frame-alist '(font . "IBM Plex Mono-11" ))

;; basic gui settings
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(setq inhibit-splash-screen t)
(setq custom-safe-themes t)
