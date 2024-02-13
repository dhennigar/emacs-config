;;; hello-world.el --- "Hello, World!" in Emacs LISP

;;; Commentary:

;; This "Hello, World!" example is written as an interactive function
;; which prompts the user for a recipient of the greeting.

;;; Code:

(defun hello-world (name)
  "Print a greeting to `NAME'."
  (interactive (list (read-string "Recipient: " "World")))
  (message (concat "Hello, " name "!")))

(provide 'hello-world)

;;; hello-world.el ends here
