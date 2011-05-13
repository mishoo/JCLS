;; foo
;; bar

(progn
  (destructuring-bind (a (b c) &optional (d (* 2 a))) (list 1 (list 2 3))
    (list a b c d)))

(progn
  (defmacro test ((a b &key (c (+ a b))) &body body)
    (jcls:print "a is" a)
    (jcls:print "b is" b)
    (jcls:print "c is" c)
    (jcls:print "body is" body))

  (test (3 4 :c 1)
        (moo)
        (man)))

(progn
  (defun y (func)
    ((lambda (x) (funcall x x))
     (lambda (y)
       (funcall func
                (lambda (&rest args)
                  (apply (funcall y y) args))))))

  (funcall (y (lambda (fact)
                (lambda (n)
                  (if (= n 1) 1
                      (* n (funcall fact (1- n)))))))
           10))

(progn
  (defun foo (a &key b (c (progn
                            (jcls:print "Evaluating C's default form")
                            (* a 2)) c-passed))
    (jcls:print "a is:" a)
    (jcls:print "b is:" b)
    (jcls:print "c is:" c)
    (jcls:print "c-passed is:" c-passed))

  (foo 10))

(progn
  (defun foo (a &optional (b (progn
                               (jcls:print "Evaluating B's default form")
                               (+ a 5)) b-passed)
              &rest c)
    (jcls:print "a is:" a)
    (jcls:print "b is:" b)
    (jcls:print "b-passed is:" b-passed)
    retue(jcls:print "c is:" c))

  (foo 10 2 3 4 5))

(progn
  (defun y (func)
    ((lambda (x) (funcall x x))
     (lambda (y)
       (funcall func
                (lambda (&rest args)
                  (apply (funcall y y) args))))))

  (funcall (y (lambda (fact)
                (lambda (n)
                  (if (= n 1) 1
                      (* n (funcall fact (1- n)))))))
           10))


((lambda (f)
   (funcall f f 5))
 (lambda (f n)
   (if (eq n 1) 1
       (* n (funcall f f (- n 1))))))


(progn

  (defun map (list proc)
    (if list
        (cons (funcall proc (car list))
              (map (cdr list) proc))))

  (defun foreach (list proc)
    (if list
        (progn
          (funcall proc (car list))
          (foreach (cdr list) proc))))

  (defun reduce (list proc init)
    (if list
        (reduce (cdr list) proc (funcall proc (car list) init))
        init))

  (defun append (a b)
    (if a
        (cons (car a) (append (cdr a) b))
        b))

  ;; quite inefficient
  (defun reverse (list)
    (if list
        (append (reverse (cdr list)) (cons (car list) nil))))

  ;; this is a LOT faster
  (defun reverse (list)
    (let ((ret nil))
      (foreach list (lambda (el)
                      (setq ret (cons el ret))))
      ret))

  (defun reverse (list)
    (reduce list (function cons) nil))

  ;; (jcls:print (reverse '(1 2 3)))

  (defun range (x)
    (if (> x 0)
        (cons x (range (- x 1)))))

  ;; (reverse (range 400))

  (defun sqr (x) (* x x))

  (map (reverse (range 1000)) (function sqr))

  ;; test comment

  )











(progn
  (defun remainder (a b)
    (if (= b 0)
        nil
        (if (< a b)
            a
            (remainder (- a b) b))))

  (defun cmmdc (a b)
    (if (= b 0)
        a
        (cmmdc b (remainder a b))))

  (cmmdc 13 16900))

;; not working, can't access free variables from a macro
(progn
  (let ((x 1))
    (defmacro foo ()
      `(jcls:print ,(setq x (+ x 1)))))

  (foo)
  (foo)
  (foo))

(progn
  (defmacro dacă (co th el)
    (jcls:print co)
    `(let ((it ,co))
       (if it ,th ,el)))

  (dacă (list 1 2 (1+ 3) "crap" (1- 4))
        `(foo ,@it)
        '(baz))

  ;; (dacă (< 3 2)
  ;;       (progn
  ;;         ;;(jcls:print "running THEN branch")
  ;;         "Okay")
  ;;       (progn
  ;;         ;;(jcls:print "running ELSE branch")
  ;;         (dacă (> 2 1)
  ;;               (progn ;; (jcls:print "And 1")
  ;;                      "Foo")
  ;;               (progn ;; (jcls:print "And two")
  ;;                      "Boo"))))
  )

(progn

  (let ((y 1))
    (defun test (x)
      (* x x (setq y (+ y 1)))))

  (defmacro foo (x)
    (let ((x (test x)))
      `(+ ,x (test ,x))))

  (foo 5))

(progn

  (defun test (x)
    (* x x))

  (defmacro foo (x)
    (let ((x (test x)))
      `(+ ,x (test ,x))))

  (foo 5))

(progn
  (defun test (x)
    `(1 ,@x ,(+ 2 3) ,@'(a b c d)
        (foo (bar ,@x))
        (mak `(1 2 ,,@x ,,(+ 1 2)))
        ,@x))
  (test (list 'crap 'mak))
  )

(progn

  (defun map (list proc)
    (if list
        (cons (funcall proc (car list))
              (map (cdr list) proc))))

  (defun foreach (list proc)
    (if list
        (progn
          (funcall proc (car list))
          (foreach (cdr list) proc))))

  (defun reduce (list proc init)
    (if list
        (reduce (cdr list) proc (funcall proc (car list) init))
        init))

  (defun append (a b)
    (if a
        (cons (car a) (append (cdr a) b))
        b))

  ;; quite inefficient
  (defun reverse (list)
    (if list
        (append (reverse (cdr list)) (cons (car list) nil))))

  ;; this is a LOT faster
  (defun reverse (list)
    (let ((ret nil))
      (foreach list (lambda (el)
                      (setq ret (cons el ret))))
      ret))

  (defun reverse (list)
    (reduce list (function cons) nil))

  ;; (jcls:print (reverse '(1 2 3)))

  (defun range (x)
    (if (> x 0)
        (cons x (range (- x 1)))))

  ;; (reverse (range 400))

  (defun sqr (x) (* x x))

  (map (reverse (range 1000)) (function sqr))

  ;; test comment

  )





(progn
  (let ((tmp 0))
    (defun count ()
      (setq tmp (+ tmp 1))))
  (jcls:print (count))
  (jcls:print (count))
  (jcls:print (count))
  (jcls:print (count)))

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
    (jcls:print (+ x y z)))
  ;;foo
  x)

(progn
  (let ((tmp 0))
    (defun count ()
      (setq tmp (+ tmp 1))))
  (jcls:print (count))
  (jcls:print (count))
  (jcls:print (count))
  (jcls:print (count)))

(progn
  (defun sqr (x)
    (* x x))
  (sqr 8))

(labels ((fact (n)
           (if (= n 1) 1
               (* n (fact (- n 1))))))
  (fact 10))

(progn
  (jcls:print ((lambda (f)
             (funcall f f 10))
           (lambda (f n)
             (if (= n 1) 1
                 (* n (funcall f f (- n 1)))))))
  (jcls:print "Check this out"))
