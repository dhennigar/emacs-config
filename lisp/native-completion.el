;;; native-completion.el --- Making the most of the built-in completion framework
;;; Author: Daniel Hennigar, 2024

;;; Commentary:

;; This makes the most of the built in completion framework - essentially trying to emulate
;; packages like Vertico that provide vertical completion buffers.

;;; Code:

(setq completion-auto-help 'always)
(setq completion-auto-select 'second-tab)

(setq completions-max-height 10)
(setq completions-format 'one-column)
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

(provide 'native-completion)

;;; native-completion.el ends here


