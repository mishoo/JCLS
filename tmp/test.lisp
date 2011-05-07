(let ((foo (lambda (a b c d)
             (+ a b c d))))
  (apply foo 1 2 '(3 4)))

(progn
  (let ((val 0))
    (defun counter ()
      (setq val (+ val 1))))

  (io:log (counter))
  (io:log (counter))
  (io:log (counter)))
