(jcls:print "Entering evaluator")

(let ((x 0))
  (defun counter ()
    (setq x (1+ x))))

(defmacro while (condition &body body)
  (let ((repeat (gensym "REPEAT")))
    `(labels ((,repeat ()
                (if ,condition
                    (progn
                      ,@body
                      (,repeat)))))
       (,repeat))))

(let ((val (counter)))
  (while (< val 10)
    (jcls:print val)
    (setq val (counter))))

(jcls:print (eq (gensym) (gensym)))
(jcls:print (gensym))
(jcls:print (gensym))
