;;; drh-completion.el --- Making the most of the built-in completion framework
;;; Author: Daniel Hennigar, 2024

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


