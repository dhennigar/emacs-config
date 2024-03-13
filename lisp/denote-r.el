;;; denote-r.el --- export denote metadata in an R-readable format

;; Copyright (C) Daniel Hennigar, 2024

;;; Commentary:

;; Given a set of notes created with Protesilaos Stavrou's
;; Denote package, this script will output a simple data
;; structure with their links and backlinks. This can be
;; used to visualize the knowledge graph in R.

;;; Code:

(require 'denote)

;; NOTES TO SELF:

;; The end structure I want is something like this:

;; '((ID . '(LINKS))
;;   (ID . '(LINKS))
;;   ... etc)

;; Then I need a way to export an emacs alist to a text
;; file that R can read in.

;; These functions can be used to generate lists of links
;; and backlinks for for a given file.
;; (denote-link-return-backlinks "FILE")
;; (denote-link-return-forelinks "FILE")

;; This one generates a hashtable of all note IDs.
;; (denote--get-all-used-ids)

(denote-region)

(provide 'denote-r)
;;; denote-r.el ends here

