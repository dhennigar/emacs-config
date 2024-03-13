;;; fibonacci.el --- A simple Fibonacci sequence generator.

;;; Commentary:

;; Copyright (C) Daniel Hennigar, 2024

;; Common LISP emulation is required for using non-nil default values
;; for optional arguments.  An interactive user-interface function is
;; also provided for printing the sequence out to the *Messages* buffer.

;;; Code:

(require 'cl-lib)
(require 's)

(cl-defun fibonacci (n &optional (a 0) (b 1))
  "Generate a fibonacci sequence of N digits.
Optional arguments A and B are used for recursive calculation."
  (unless (zerop n)
    (cons a (fib (1- n) b (+ a b)))))

;;;###autoload
(defun fibonacci-print (n)
  "Interactive function to print a sequence of N Fibonacci numbers."
  (interactive (list (read-number "N: ")))
  (message (s-replace-all '(("(" . "") (")" . ""))
			  (format "Fibonacci sequence: %s" (fibonacci n)))))

(provide 'fibonacci)

;;; fibonacci.el ends here
