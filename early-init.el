;;; early-init.el --- My Emacs configuration loaded early in init

;; Copyright (c) 2022-2023 Daniel Hennigar

;; Author: Daniel Hennigar
;; URL: https://github.com/dhennigar/emacs-config
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 or any later version.

;; This file is distributed in the hope that it will be usefule,
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; My Emacs configuration loaded early in init.

;;; Code:

;; -----------------------------------------------------------------------------
;; Early init

;; avoid garbage collection during init
(setq gc-cons-threshold (* 10 1000 1000 1000))

;;enable this for troubleshooting startup times.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract
                                 after-init-time before-init-time)))
                     gcs-done)))

;; do not load packages until I say so
(setq package-enable-at-startup nil)

;; add custom lisp directory to load path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; use a custom custom file
(setq custom-file (concat user-emacs-directory "custom.el"))

;; backups are stored in the system temp directory
(setq
 backup-directory-alist '((".*" . "~/.emacs.d/backups/"))
 delete-old-versions t
 version-control t)

;; I don't need autosave files
(setq auto-save-default nil)

;; GUI settings for early init
(setq custom-safe-themes t)

;; Default frame settings
(setq default-frame-alist
      '((font . "IBM Plex Mono-10")
	(height . 40)
	(width . 84)
	(left-fringe . 0)
	(right-fringe . 0)))
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq tool-bar-mode nil)
(set-scroll-bar-mode nil)
(setq menu-bar-mode nil)

;; smooth scrolling
(pixel-scroll-precision-mode)
(setq pixel-scroll-precision-use-momentum t)

;; more minimal scratch message
(setq initial-scratch-message
      ";; Emacs LISP *scratch* buffer\n\n")

;; I want small window margins
(add-hook 'window-configuration-change-hook
          (lambda ()
            (set-window-margins
	     (car (get-buffer-window-list
		   (current-buffer) nil t)) 1 1)))

;; I don't want annoying beeps
(setq ring-bell-function 'ignore)

;;; early-init.el ends here
