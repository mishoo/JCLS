(let* ((a 5)
       (b (+ a 3))
       (c (* a b)))
  (jcls:print a b c)
  (setq c 1234)
  (jcls:print c))

(let ((foo 5))
  (let ((foo 1)
        (bar (+ foo 1)))
    (+ foo bar)))

(labels ((add (x y) (+ x y))
         (mul (x y) (* x y))
         (test (x y) (add (mul x y) 1)))
  (jcls:print (add 10 (mul 2 3)))
  (jcls:print (test 2 3)))

;; (labels ((sum (n)
;;            (if (= n 0) 0
;;                (+ n (sum (1- n))))))
;;   (jcls:print (sum 10000)))

(jcls:print (case 2
              (1 "foo")
              ((2 4 5) "bar")
              (3 "baz")))

(jcls:print (macroexpand-1 '(or 1 2 3)))



(gensym)
