((lambda (x y z) (+ x (* y z) 2))
 10 20 30)

((lambda (f)
   (funcall f f 5))
 (lambda (f n)
   (if (eq n 1) 1
       (* n (funcall f f (- n 1))))))
