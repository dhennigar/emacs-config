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
;; Debugging tools

;; ;; Show full debug when an elisp function errors out.
;; (setq debug-on-error t)

;; Time the startup and report with a message
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract
                                 after-init-time before-init-time)))
                     gcs-done)))


;; -----------------------------------------------------------------------------
;; Optimization

;; Avoid garbage collection during init
(setq gc-cons-threshold (* 10 1000 1000 1000))


;; -----------------------------------------------------------------------------
;; File paths

;; Personal lisp files
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Use a custom custom file
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Backups are stored in a backup directory
(setq
 backup-directory-alist '((".*" . "~/.emacs.d/backups/"))
 delete-old-versions t
 version-control t)


;; -----------------------------------------------------------------------------
;; Aesthitic settings

;; Default fonts
(set-face-attribute 'default nil
		    :family "Iosevka Comfy"
		    :weight 'regular
		    :height 120)

;; Default frame settings
(setq default-frame-alist
      '((height . 40)
	(width . 100)
	(left-fringe . 0)
	(right-fringe . 0)))
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq tool-bar-mode nil)
(scroll-bar-mode -1)
(setq menu-bar-mode nil)

;; Show column and row numbers in mode line
(setq column-number-mode 1)

;; Smooth scrolling
(pixel-scroll-precision-mode)
(setq pixel-scroll-precision-use-momentum nil)

;; Minimal scratch message
(setq initial-scratch-message
      ";; Emacs LISP *scratch* buffer\n\n")

;; Small window margins
(add-hook 'window-configuration-change-hook
          (lambda () (set-window-margins
		      (car (get-buffer-window-list
			    (current-buffer) nil t)) 1 1)))

(setq blink-cursor-blinks 0) ;; blink forever

;; -----------------------------------------------------------------------------
;; Things I don't want

;; I don't need autosave files
(setq auto-save-default nil)

;; Custom themes require no confirmation
(setq custom-safe-themes t)

;; No annoying beeps
;; I think this is only necessary on Windows but doesn't hurt
(setq ring-bell-function 'ignore)

;;; early-init.el ends here
