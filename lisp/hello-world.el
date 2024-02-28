;;; hello-world.el --- "hello, world" in Emacs LISP

;;; Copyright (C) Daniel Hennigar 2024

;;; Commentary:

;; This "hello, world" example is written as an interactive function
;; which prompts the user for a recipient of the greeting.

;;; Code:

(defun hello-world (&optional  name)
  "Print a \"Hello, World!\" message.
\nOptionally address the greeting to NAME if called with a prefix."
  (interactive
   (if current-prefix-arg
       (list (read-string "Recipient: "))
     '("World")))
  (unless name
    (setq name "World"))
  (message (format "Hello, %s!" name)))

(provide 'hello-world)

;;; hello-world.el ends here
