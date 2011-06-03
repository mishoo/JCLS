(export 'not)
(defmacro not (condition)
  `(if ,condition nil t))

(export 'when)
(defmacro when (condition &body body)
  `(if ,condition
       (progn ,@body)))

(export 'unless)
(defmacro unless (condition &body body)
  `(when (not ,condition)
     ,@body))

(export 'cond)
(defmacro cond (cases)
  (if cases
      `(if ,(caar cases)
           (progn ,@(cdar cases))
           (cond ,(cdr cases)))))

(export 'or)
(defmacro or (&rest exprs)
  (when exprs
    (let ((ex (gensym "OR")))
      `(let ((,ex ,(car exprs)))
         (if ,ex ,ex (or ,@(cdr exprs)))))))

(export 'and)
(defmacro and (&rest exprs)
  (if exprs
      (let ((ex (gensym "AND")))
        `(let ((,ex ,(car exprs)))
           (when ,ex
             ,(if (cdr exprs) `(and ,@(cdr exprs)) ex))))
      t))

(export 'case)
(defmacro case (expr &rest cases)
  (let ((vexpr (gensym "CASE")))
    `(let ((,vexpr ,expr))
       ,(labels ((recur (cases)
                        (when cases
                          (if (listp (caar cases))
                              `(if (member ,vexpr ',(caar cases))
                                   (progn ,@(cdar cases))
                                   ,(recur (cdr cases)))
                              (if (and (not (cdr cases))
                                       (or (eq (caar cases) 'otherwise)
                                           (eq (caar cases) t)))
                                  `(progn ,@(cdar cases))
                                  `(if (eq ,vexpr ',(caar cases))
                                       (progn ,@(cdar cases))
                                       ,(recur (cdr cases))))))))
                (recur cases)))))

(export 'macroexpand-1)
(defun macroexpand-1 (form &optional env)
  (if (symbolp (car form))
      (progn
        (let ((func (macro-function (car form) env)))
          (apply (funcall func env) (cdr form))))))
