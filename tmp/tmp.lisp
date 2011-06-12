(jcls:import 'jcls:print)

(tagbody
 t1
   (print 't1)
   (go t3)
 t2
   (print 't2)
   (labels ((foo ()
              (print 'in-foo)
              (go t4)))
     (foo))
   (print "won't reach here")
 t3
   (print 't3)
   (go t2)
 t4
   (print 't4))

;; (defparameter *foo* 10)

;; (let ((*foo* 5))
;;   *foo*)

;; *foo*

;; (let ((moo 5))
;;   (jcls:print "return value:"
;;               (unwind-protect
;;                    (progn
;;                      (jcls:print "here moo is " 5)
;;                      moo)
;;                 (setq moo 7)))
;;   (jcls:print moo))

;; (let* ((a 5)
;;        (b (+ a 3))
;;        (c (* a b)))
;;   (jcls:print a b c)
;;   (setq c 1234)
;;   (jcls:print c))

;; (let ((foo 5))
;;   (let ((foo 1)
;;         (bar (+ foo 1)))
;;     (+ foo bar)))

;; (labels ((add (x y) (+ x y))
;;          (mul (x y) (* x y))
;;          (test (x y) (add (mul x y) 1)))
;;   (jcls:print (add 10 (mul 2 3)))
;;   (jcls:print (test 2 3)))

;; (labels ((sum (n)
;;            (if (= n 0) 0
;;                (+ n (sum (1- n))))))
;;   (jcls:print (sum 10000)))

;; (jcls:print (case 2
;;               (1 "foo")
;;               ((2 4 5) "bar")
;;               (3 "baz")))

;; (jcls:print (macroexpand-1 '(or 1 2 3)))



;; ;; (gensym)
