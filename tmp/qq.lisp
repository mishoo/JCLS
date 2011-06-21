(jcls:import '(jcls:print))

(print (macroexpand-1 `(flet ((dbl (x)
                                (* x x))
                              (inc (x)
                                (1+ x)))
                         (dbl (inc 4)))))

(flet ((dbl (x)
         (* 2 x))
       (inc (x)
         (1+ x)))
  (dbl (inc 4)))
