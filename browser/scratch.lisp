;; Type Lisp here and use the keybindings:
;;
;; - «C-c C-k» to evaluate the whole buffer
;; - «M-C-x», or «C-c C-c», to evaluate the current toplevel expression
;; - «C-c C-r» to evaluate the selection
;; - «C-c ENTER» to macroexpand-1 the current expression (the cursor must be on the open paren)
;; - «C-c DELETE» to clear the output buffer
;; - «C-x C-s» to save the current buffer (in localStorage)
;; - «C-x C-f» to load a file from localStorage
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
