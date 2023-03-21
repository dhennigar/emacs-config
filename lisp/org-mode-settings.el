;; org-mode-settings.el

;; bibtex citations in org-mode
;;(use-package helm-bibtex
;;  :custom ((bibtex-completion-bibliography "~/Zotero/zotero.bib") ;; this is not correct for Windows! (Unless I move it)
;;	   (bibtex-completion-pdf-field "File"))
;;  )

;; org-mode settings
(with-eval-after-load 'org       
  (setq org-startup-indented t)
  (add-hook 'org-mode-hook #'visual-line-mode))
