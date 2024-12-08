;;; tree-sitter-grammars.el --- list of repositories for tree-sitter languages

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
