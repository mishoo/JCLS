;; Type Lisp here and use the keybindings:
;;
;; - C-c C-k -- evaluate the whole buffer
;; - M-C-x (or C-c C-c) -- evaluate the current toplevel expression
;; - C-c C-r -- evaluate the selection
;; - C-c ENTER -- macroexpand-1 the current expression (the cursor must be on the open paren)
;; - C-c DELETE -- clear the output buffer
;; - C-x C-s -- save the current buffer (in localStorage)
;; - C-x C-f -- load a file from localStorage
;;
;; To load some ready-made samples, click “Load sample”

(in-package :cl-user)

(defun sum (n)
  (if (= n 0)
      0
      (+ n (sum (1- n)))))

(sum 1000) ;; try a bigger value here
           ;; the stack won't be exhausted
           ;; for 30000 it takes 2s with Chrome
