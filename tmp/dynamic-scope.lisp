(import '(jcls:print))

(defparameter *x* 1)

(defun foo ()
  (print *x*))

(defun test ()
  (foo))

(defun bar ()
  (dlet ((*x* 5))
    (test)))

(defun baz (*x*)
  (foo))

(foo)
(bar)
(baz 10)
(foo)
