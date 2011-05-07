;; check

(progn
  (let ((tmp 0))
    (defun count ()
      (setq tmp (+ tmp 1))))
  (io:log (count))
  (io:log (count))
  (io:log (count))
  (io:log (count)))

(progn
  (defun sqr (x)
    (* x x))
  (sqr 8))

(labels ((fact (n)
           (if (eq n 1) 1
               (* n (fact (- n 1))))))
  (fact 10))

(progn
  (io:log ((lambda (f)
             (funcall f f 10))
           (lambda (f n)
             (if (= n 1) 1
                 (* n (funcall f f (- n 1)))))))
  (io:log "Check this out"))
