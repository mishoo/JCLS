(labels ((fact (n)
           (if (eq n 1) 1
               (* n (fact (- n 1))))))
  (fact 10))

(flet ((foo (x)
         (+ x 1)))
  (foo 10))
