;;; completions.el -- native completion framework

;;; Commentary:

;; This configuration is borrowed from Robert Enzmann <https://robbmann.io/posts/emacs-29-completions>
;; This configuration purports to render the built-in completion framework usable

;;; Code:

(setq completion-auto-help 'always)
(setq completion-auto-select 'second-tab)

(setq completions-max-height 10)
(setq completions-header-format nil)
(setq completion-show-help nil)

(define-key minibuffer-mode-map (kbd "C-n")
	    #'minibuffer-next-completion)
(define-key minibuffer-mode-map (kbd "C-p")
	    #'minibuffer-previous-completion)

(define-key completion-in-region-mode-map (kbd "C-n")
	    #'minibuffer-next-completion)
(define-key completion-in-region-mode-map (kbd "C-p")
	    #'minibuffer-previous-completion)

;;; completions.el ends here
