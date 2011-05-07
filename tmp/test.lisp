;; check

(progn
  (let ((val 0))
    (defun counter ()
      (setq val (+ val 1))))

  (io:log (counter))
  (io:log (counter))
  (io:log (counter)))
