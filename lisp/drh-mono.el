;;; drh-mono-themes.el --- Almost monochromatic color themes -*- lexical-binding: t; -*-

;; Copyright (C) 2019 - 2022 John Olsson
;; Copyright (C) 2024 Daniel Hennigar

;; Original author of almost-mono-themes:
;; Author: John Olsson <john@cryon.se>
;; Maintainer: John Olsson <john@cryon.se>
;; URL: https://github.com/cryon/almost-mono-themes

;; Author of personalized drh-mono-themes:
;; Author: Daniel Hennigar <drhenn@proton.me>
;; Maintainer: Daniel Hennigar <drhenn@proton.me>
;; URL: https://github.com/dhennigar/drh-mono-themes

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A suite of almost monochrome Emacs themes updated by DRH

;; TODO: Add faces for elfeed, ess

;;; Code:

(defconst drh-mono-themes-colors
	'((white
	   (background . "#ffffff")
	   (foreground . "#000000")
	   (weak . "#888888")
	   (weaker . "#dddddd")
	   (weakest . "#efefef")
	   (highlight . "gray88")
	   (warning . "#ff0000")
	   (success . "#00ff00")
	   (string . "dark green"))
	  (black
	   (background . "#000000")
	   (foreground . "#ffffff")
	   (weak . "#aaaaaa")
	   (weaker . "#666666")
	   (weakest . "#222222")
	   (highlight . "gray33")
	   (warning . "#ff0000")
	   (success . "#00ff00")
	   (string . "dark sea green"))
	  (gray
	   (background . "#2b2b2b")
	   (foreground . "#ffffff")
	   (weak . "#aaaaaa")
	   (weaker . "#666666")
	   (weakest . "#222222")
	   (highlight . "dim gray")
	   (warning . "#ff0000")
	   (success . "#00ff00")
	   (string . "salmon"))
	  (acme
	   (background . "#fefdcf")
	   (foreground . "#000000")
	   (weak . "#559955")
	   (weaker . "#aaaaaa")
	   (weakest . "#bbeeff")
	   (highlight . "lightgoldenrod")
	   (warning . "#ff0000")
	   (success . "#00ff00")
	   (string . "firebrick"))
	  (cream
	   (background . "#f0e5da")
	   (foreground . "#000000")
	   (weak . "#7d7165")
	   (weaker . "#c4baaf")
	   (weakest . "#dbd0c5")
	   (highlight . "AntiqueWhite3")
	   (warning . "#ff0000")
	   (success . "#00ff00")
	   (string . "firebrick"))))

(defmacro drh-mono-themes--variant-with-colors (variant &rest body)
  "Execute BODY in a scope where the different colors for given VARIANT is bound."
  `(let* ((colors (or (cdr (assoc ,variant drh-mono-themes-colors))
		      (error "No such theme variant")))
	  (background (cdr (assoc 'background colors)))
	  (foreground (cdr (assoc 'foreground colors)))
	  (weak	      (cdr (assoc 'weak colors)))
	  (weaker     (cdr (assoc 'weaker colors)))
	  (weakest    (cdr (assoc 'weakest colors)))
	  (highlight  (cdr (assoc 'highlight colors)))
	  (warning    (cdr (assoc 'warning colors)))
	  (success    (cdr (assoc 'success colors)))
	  (string     (cdr (assoc 'string colors))))
     ,@body))

(defmacro drh-mono-themes--faces-spec ()
  "Provide the faces specification."
  (quote
   (mapcar
    (lambda (entry) (list (car entry) `((t ,@(cdr entry)))))
    `(

      ;; default
      (default (:background ,background :foreground ,foreground))
      (fringe  (:background ,background))
      (region  (:background ,highlight  :foreground ,foreground))
      (show-paren-match (:weight extra-bold))
      (show-paren-mismatch (:foreground ,warning :weight extra-bold))
      (minibuffer-prompt (:weight bold :foreground ,foreground))
      (isearch (:background ,weak :foreground ,foreground :bold t))
      (lazy-highlight (:background ,weaker :foreground ,foreground))
      (link (:underline t))
      (highlight (:background ,highlight :foreground ,foreground))

      ;; mode line
      (mode-line (:box (:line-width -1 :color ,weaker)
		       :background ,weakest :foreground ,foreground))

      (mode-line-inactive (:box (:line-width -1 :color ,weaker)
				:background ,background :foreground ,weaker))

      ;; font lock
      (font-lock-keyword-face (:bold t))
      (font-lock-function-name-face (:bold t))
      (font-lock-variable-name-face (:foreground ,foreground))
      (font-lock-warning-face
       (:foreground ,foreground :underline (:color ,warning :style wave)))
      (font-lock-builtin-face (:bold t))
      (font-lock-variable-name-face (:foreground ,foreground :italic t))
      (font-lock-constant-face (:bold t :italic t))
      (font-lock-type-face (:italic t))
      (font-lock-preprocessor-face (:italic t))
      (font-lock-comment-face (:foreground ,weak :italic t))
      (font-lock-string-face (:foreground ,string))
      (font-lock-doc-face (:inherit font-lock-comment-face))
      (line-number (:foreground ,weaker))
      (linum (:inherit line-number))
      (vertical-border (:foreground ,weaker))

      ;; eshell
      (eshell-prompt (:foreground ,foreground :bold t))
      (eshell-ls-directory (:foreground ,foreground :bold t))
      (eshell-ls-archive (:inherit eshell-ls-unreadable))
      (eshell-ls-backup (:inherit eshell-ls-unreadable))
      (eshell-ls-clutter (:inherit eshell-ls-unreadable))
      (eshell-ls-executable (:inherit eshell-ls-unreadable))
      (eshell-ls-missing (:inherit eshell-ls-unreadable))
      (eshell-ls-product (:inherit eshell-ls-unreadable))
      (eshell-ls-readonly (:inherit eshell-ls-unreadable))
      (eshell-ls-special (:inherit eshell-ls-unreadable))
      (eshell-ls-symlink (:inherit eshell-ls-unreadable))

      ;; sh-mode
      (sh-quoted-exec (:foreground ,foreground :italic nil))

      ;; corfu mode
      (corfu-current (:background ,highlight :foreground ,foreground))

      ;; company mode
      (company-tooltip (:background ,weakest :foreground ,foreground))
      (company-tooltip-selection (:background ,weaker :foreground ,foreground))
      ;;(company-tooltip-search (:background "#ff0000" :foreground "#00ff00"))
      (company-tooltip-common (:bold t))
      (company-tooltip-common-selection (:bold t))
      (company-scrollbar-bg (:background ,weaker))
      (company-scrollbar-fg (:background ,weak))
      (company-tooltip-annotation-selection (:background ,weaker :foreground ,foreground :italic t))
      (company-tooltip-annotation (:background ,weakest :foreground ,weak :italic t))

      ;; git gutter
      (git-gutter:modified (:background ,highlight :foreground ,highlight))
      (git-gutter:added (:background ,success :foreground ,success))
      (git-gutter:deleted (:background ,warning :foreground ,warning))

      ;; diff hl
      (diff-hl-change (:background ,highlight :foreground ,highlight))
      (diff-hl-insert (:background ,success :foreground ,success))
      (diff-hl-delete (:background ,warning :foreground ,warning))

      ;; hl line
      (hl-line (:background ,weakest))
      (highlight-current-line-face (:inherit hl-line))

      ;; ido
      (ido-first-match (:bold t))
      (ido-only-match (:bold t))
      (ido-subdir (:italic t))
      (ido-virtual (:foreground ,weak))
      (ido-vertical-match-face (:bold t :italic nil))

      ;; org mode
      (org-table (:foreground ,weak))
      (org-done (:foreground "green" :background ,background :bold t))      
      (org-headline-done (:foreground ,weak))
      (org-todo (:foreground "red" :background ,background :bold t))
      (org-headline-todo (:foreground ,foreground))

      ;; elfeed
      (elfeed-search-feed-face (:foreground ,string :bold t))
      (elfeed-search-tag-face (:foreground ,weak))

      ;; Org mode
      (org-level-1 (:bold t))      
      (org-level-2 (:bold t))      
      (org-level-3 (:bold t))

      ))))

(defun drh-mono-themes--variant-name (variant)
  "Create symbol for color theme variant VARIANT."
  (intern (format "drh-mono-%s" (symbol-name variant))))

(defmacro drh-mono-themes--define-theme (variant)
  "Define a theme for the drh-mono variant VARIANT."
  (let ((name (drh-mono-themes--variant-name variant))
        (doc (format "drh mono theme (%s version)" variant)))
    `(progn
       (deftheme ,name ,doc)
       (put ',name 'theme-immediate t)
       (drh-mono-themes--variant-with-colors
        ',variant
        (apply 'custom-theme-set-faces ',name
               (drh-mono-themes--faces-spec)))
       (provide-theme ',name))))

(defun drh-mono-themes--load-theme (theme)
  "Unload themes before loading a new THEME."
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme theme))

(defun drh-mono-themes-toggle ()
  "Toggle between white and black drh-mono themes."
  (interactive)
  ;; Placeholder
  (cond
   ((member 'drh-mono-black custom-enabled-themes)
    (drh-mono-themes--load-theme 'drh-mono-white))
   ((member 'drh-mono-white custom-enabled-themes)
    (drh-mono-themes--load-theme 'drh-mono-black))
   (t (drh-mono-themes--load-theme 'drh-mono-white))))

(defun drh-mono-themes-toggle-tinted ()
  "Toggle between cream and gray drh-mono-themes."
  (interactive)
  (cond
   ((member 'drh-mono-gray custom-enabled-themes)
    (drh-mono-themes--load-theme 'drh-mono-cream))
   ((member 'drh-mono-cream custom-enabled-themes)
    (drh-mono-themes--load-theme 'drh-mono-gray))
   (t (drh-mono-themes--load-theme 'drh-mono-cream))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory (file-name-directory load-file-name))))

(provide 'drh-mono)

;;; drh-mono-themes.el ends here
