;; (let ((x 42))
;;   (jcls:print (* x x)))

;; (jcls:print "hello world")

;; (if nil 'a 'b)

;; (lambda (a b) (+ a b))

;; (progn
;;   'a
;;   'b
;;   'c)

((lambda (a &optional (b (* 2 a)) &key (c (/ b 2)))
   (funcall (function *) a b c))
 2 3 :c 4)

;; (defun foo (a b)
;;   (+ a b))
