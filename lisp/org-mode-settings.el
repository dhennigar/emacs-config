;; org-mode-settings.el

;; bibtex citations in org-mode
(use-package helm-bibtex
  :custom ((bibtex-completion-bibliography "~/Zotero/bibtex/bibtex.bib")
	   (bibtex-completion-pdf-field "file")))

;; org-mode settings
(with-eval-after-load 'org       
  (setq org-startup-indented t)
  (add-hook 'org-mode-hook #'visual-line-mode))

;; pdf-tools
(use-package pdf-tools)
