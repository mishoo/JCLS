;; check

((lambda (f)
   (funcall f f 10))
 (lambda (f n)
   (if (= n 1) 1
       (* n (funcall f f (- n 1))))))
