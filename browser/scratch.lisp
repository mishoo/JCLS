;; Type Lisp here and use the keybindings: (in Emacs parlance, that is,
;; C stands for CTRL, M stands for META which is usually the ALT key)
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

;; If you go to the REPL (the buffer below this one) you can type an
;; expression and press ENTER to evaluate it right there.  To quickly
;; move to another frame, use M-ARROWS (for example M-ARROW_DOWN would
;; move below); or just click there.

;; For more keybindings take a look at
;; http://www.ymacs.org/userdocs.html
