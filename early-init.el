;;; early-init.el --- My Emacs configuration loaded early in init

;; Copyright (c) 2022-2023 Daniel Hennigar

;; Author: Daniel Hennigar
;; URL: https://github.com/dhennigar/emacs-config
;; Package-Requires: ((emacs "28.2"))

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

;; Early init ---------------------------------------------------------

(setq package-enable-at-startup nil)

(add-to-list 'default-frame-alist '(font . "IBM Plex Mono-11" ))
(add-to-list 'default-frame-alist '(height . 26))
(add-to-list 'default-frame-alist '(width . 84))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(fringe-mode 0)

(setq custom-safe-themes t)

(add-hook 'window-configuration-change-hook
          (lambda ()
            (set-window-margins
	     (car (get-buffer-window-list (current-buffer) nil t)) 1 1)))

(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)

;; backups are stored in the system temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; early-init.el ends here
