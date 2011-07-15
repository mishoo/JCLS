(jcls:print (macroexpand-1 '(defpackage foo
                             (:use :common-lisp)
                             (:use :jcls)
                             (:export 'moo))))

(defpackage foo (:use :cl :jcls))

(in-package :foo)

(print "This seems to work")
