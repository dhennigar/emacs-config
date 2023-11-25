;;; tree-sitter-grammars.el --- define compile instructions for tree-sitter

;;; Commentary:

;; Add further grammar locations as necessary.

;;; Code:

(setq treesit-language-source-alist
      '((markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(bash "https://github.com/tree-sitter/tree-sitter-bash")))


