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

(progn

  (defun map (list proc)
    (if list
        (cons (funcall proc (car list))
              (map (cdr list) proc))))

  (defun append (a b)
    (if a
        (cons (car a) (append (cdr a) b))
        b))

  ;; quite inefficient
  (defun reverse (list)
    (if list
        (append (reverse (cdr list)) (cons (car list) nil))))

  (io:log (reverse '(1 2 3)))

  (defun range (x)
    (if (> x 0)
        (cons x (range (- x 1)))))

  (map (reverse (range 100))
       (lambda (x)
         (* x x)))

  ;; test comment

  )





(progn
  (let ((tmp 0))
    (defun count ()
      (setq tmp (+ tmp 1))))
  (io:log (count))
  (io:log (count))
  (io:log (count))
  (io:log (count)))

(let ((x 10))
  (setq x 20)
  x)

(labels ((fact (n)
           (if (= n 1) 1
               (* n (fact (- n 1))))))
  (fact 10))

(let ((x 10))
  (let* ((y x)
         (x (+ x x))
         (z (* x 5)))
    (io:log (+ x y z)))
  ;;foo
  x)

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
           (if (= n 1) 1
               (* n (fact (- n 1))))))
  (fact 10))

(progn
  (io:log ((lambda (f)
             (funcall f f 10))
           (lambda (f n)
             (if (= n 1) 1
                 (* n (funcall f f (- n 1)))))))
  (io:log "Check this out"))
