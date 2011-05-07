((lambda (x) (+ (funcall x) 1))
 (lambda () 5))

((lambda (x) (+ (funcall x 2) 1))
 (lambda (z) (* z 5)))

((lambda (f)
   (funcall f f 5))
 (lambda (f n)
   (if (eq n 1) 1
       (* n (funcall f f (- n 1))))))

((lambda (x y z) (+ x (* y z) 2))
 10 20 30)

(let* ((x 10)
       (y (+ x 1)))
  (+ x y))

(labels ((fact (n)
           (if (eq n 1) 1
               (* n (fact (- n 1))))))
  (fact 10))

(flet ((foo (x)
         (+ x 1)))
  (foo 10))
