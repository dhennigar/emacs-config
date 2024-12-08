;;; drh-completion.el --- Making the most of the built-in completion framework

;; Copyright (c) 2022-2024 Daniel Hennigar

;; Author: Daniel Hennigar
;; URL: https://github.com/dhennigar/emacs-config
;; Package-Requires: ((emacs "29.4"))

;; This file is NOT part of GNU Emacs

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 or any later version.

;; This file is distributed in the hope that it will be useful,
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; These settings make the built-in completions system usable.

;;; Code:

;; Tab will either indent or attempt completion based on context
(setq tab-always-indent 'complete)
(setq tab-first-completion nil)

;; Hitting tab a second time enters the *Completions* buffer
;; Repeated tabs select the next candidate in the list
(setq completion-auto-select 'second-tab)

;; Show *Completions* buffer after failed (incomplete) completions
;; and stay visible until I select a candidate
(setq completion-auto-help 'visible)

;; Completion candidates are shown in a vertical list of height 10
(setq completions-format 'one-column)
(setq completions-max-height 10)

;; No explanatory text is needed in the *Completions* buffer
(setq completions-header-format nil)
(setq completion-show-help nil)

;; Use C-n and C-p to select completion candidates
(define-key minibuffer-mode-map (kbd "C-n")
	    #'minibuffer-next-completion)
(define-key minibuffer-mode-map (kbd "C-p")
	    #'minibuffer-previous-completion)
(define-key completion-in-region-mode-map (kbd "C-n")
	    #'minibuffer-next-completion)
(define-key completion-in-region-mode-map (kbd "C-p")
	    #'minibuffer-previous-completion)

;; Appearance and placement of *Completions* buffer
(setq display-buffer-alist
      '(("\\*Completions\\*"
	 (display-buffer-reuse-mode-window display-buffer-at-bottom)
	 (window-parameters . ((mode-line-format . none))))))


(provide 'drh-completion)
;;; drh-completion.el ends here


