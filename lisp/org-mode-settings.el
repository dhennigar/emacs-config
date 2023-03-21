;; org-mode-settings.el

;; org-mode settings
(with-eval-after-load 'org       
  (setq org-startup-indented t)
  (add-hook 'org-mode-hook #'visual-line-mode))

;; bibtex citations in org-mode
(use-package helm-bibtex
  :custom ((bibtex-completion-bibliography "~/Zotero/zotero.bib")
	   (bibtex-completion-pdf-field "File"))
  )
