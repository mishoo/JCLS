(defpackage js
  (:use :cl)
  (:import-from :jcls :def-emac :js-name-to-lisp :lisp-name-to-js)
  (:import-from :cl :def-efun))

(in-package :js)

(def-efun strcat (&rest strings)
  (apply (function jcls:string+) strings))

(def-emac with-methods ((object &rest names) &body body)
  (let ((obj (gensym)))
    `(let ((,obj ,object))
       (flet (,@(map (lambda (name)
                       `(,(intern (js-name-to-lisp name)) (&rest args)
                          (jcls:apply-native (jcls:@ ,obj ,name) ,obj args)))
                     names))
         ,@body))))
