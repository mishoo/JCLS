(jcls:print (macroexpand-1 '(defpackage foo
                             (:use :common-lisp)
                             (:use :jcls)
                             (:export "MOO"))))

(defpackage foo
  (:use :cl :jcls)
  (:export "TEST"))

(in-package :foo)

(defun test ()
  (print "This is FOO:TEST"))

(in-package :cl-user)

(foo:test)
