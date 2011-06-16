(jcls:import '(jcls:print))

`(foo bar ,(+ 2 3))

;;`(foo `(bar ,,(+ 2 3) ,(+ 2 3)))
(print ``(foo ,(+ 2 3) ,@(list 1 2 3 4) ,@(list 'moo 'man)))

(print (let ((tmp (gensym "CASE")))
         `(let ((,tmp 1))
            ,(labels ((recur (list)
                             (when list `((',(car list) . 1)
                                          ,@(recur (cdr list))))))
                     (recur '(a b c d))))))

(jcls:print (macroexpand-1 '(and (not 1)
                             )))

(print (macroexpand-1 '(let ((a 10) (b 20) (foo 30)) (+ a b foo))))

(print ``(foo ,(+ 2 3) ,@(list 1 2 3 4) ,@(list 'moo 'man)))

(print (macroexpand-1 (macroexpand-1 '(prog1 foo bar))))

(print (macroexpand-1 '(case foo ((foo bar) 1) (mak 2) ((1 2) 3))))

(let ((a '(1 2 3 4)))
  (print (eq a (nconc a '(a b c d)))))
