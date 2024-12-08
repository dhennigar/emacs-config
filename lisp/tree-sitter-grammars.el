;;; tree-sitter-grammars.el --- list of repositories for tree-sitter languages

;;; Commentary:

;; Tree-sitter is a modern framework for parsing language syntax in an
;; efficient and flexible way. I don't currently make use of tree-sitter
;; in my configuration. However, if I do decide to experiment in future,
;; below is a list tree-sitter "grammars" for languages I would be
;; interested in using.

;;; Code:

(setq treesit-language-source-alist
      '((markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(bash "https://github.com/tree-sitter/tree-sitter-bash")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(r "https://github.com/r-lib/tree-sitter-r")))

(provide tree-sitter-grammars)
;;; tree-sitter-grammars.el ends here
