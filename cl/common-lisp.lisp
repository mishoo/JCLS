(jcls:set! *package* "v" (jcls:find-package "JCLS"))

(defmacro def-f! (name args &body body)
  `(def! ,name "f" (lambda ,args ,@body)))

(export 'def-emac)
(defmacro def-emac (name args &body body)
  `(progn
     (export ',name)
     (defmacro ,name ,args ,@body)))

(def-emac in-package (name)
  `(set! *package* "v" (find-package ,name)))

(def-emac with-cc ((var) &body body)
  `(call/cc (lambda (,var)
              ,@body)))

(in-package :cl)

(jcls:import '(jcls::fork-environment
               jcls::def!
               jcls::set!
               jcls::def-f!
               jcls::def-emac
               jcls::in-package
               jcls::export
               jcls::special!
               jcls::special?))

(def-emac when (condition &body body)
  `(if ,condition
       (progn ,@body)))

(def-emac let* (defs &body body)
  (fork-environment
   (def-f! recur (defs)
     (if defs
         (cons `(def! ,(caar defs) "v" ,(cadar defs))
               (recur (cdr defs)))))
   `(fork-environment
     ,@(recur defs)
     ,@body)))

(def-emac let (defs &body body)
  (fork-environment
   (def! names "v" nil)
   (def! values "v" nil)
   (def-f! recur (defs)
     (when defs
       (set! names "v" (cons (caar defs) names))
       (set! values "v" (cons (cadar defs) values))
       (recur (cdr defs))))
   (recur defs)
   `((lambda ,names ,@body) ,@values)))

(def-emac labels (defs &body body)
  (fork-environment
   (def-f! recur (defs)
     (if defs
         (cons `(def-f! ,(caar defs) ,(cadar defs) ,@(cddar defs))
               (recur (cdr defs)))))
   `(fork-environment
     ,@(recur defs)
     ,@body)))

(def-emac flet (defs &body body)
  (fork-environment
   (def! names "v" nil)
   (def! values "v" nil)
   (def-f! recur (defs)
     (when defs
       (set! names "v" (cons (caar defs) names))
       (set! values "v" (cons `(lambda ,(cadar defs) ,@(cddar defs)) values))
       (recur (cdr defs))))
   (recur defs)
   (let ((tmp (gensym "FLET")))
     `((lambda (&rest ,tmp)
         ,@(map (lambda (name)
                  `(progn
                     (def! ,name "f" (car ,tmp))
                     (setq ,tmp (cdr ,tmp))))
                names)
         ,@body) ,@values))))

(def-emac setq (&rest defs)
  (labels ((recur (defs)
             (if defs
                 (cons `(set! ,(car defs) "v" ,(cadr defs))
                       (recur (cddr defs))))))
    `(progn ,@(recur defs))))

(def-emac defun (name args &body body)
  `(def! ,name "f" (lambda ,args ,@body) t))

(def-emac defparameter (name value &optional doc)
  `(progn
     (jcls:def! ,name "v" nil t)
     (jcls:special! ,name)
     (setq ,name ,value)))

(def-emac prog1 (first &body body)
  (let ((v1 (gensym)))
    `(let ((,v1 ,first))
       ,@body
       ,v1)))

;; XXX: SETF required for INCF/DECF
(def-emac incf (name &optional (delta 1))
  `(setq ,name (1+ ,name)))

(def-emac decf (name &optional (delta 1))
  `(setq ,name (1- ,name)))

(defmacro def-efun (name args &body body)
  `(progn
     (export ',name)
     (defun ,name ,args ,@body)))



(def-emac not (condition)
  `(if ,condition nil t))

(def-emac unless (condition &body body)
  `(when (not ,condition)
     ,@body))

(def-emac cond (cases)
  (if cases
      `(if ,(caar cases)
           (progn ,@(cdar cases))
           (cond ,(cdr cases)))))

(def-emac or (&rest exprs)
  (when exprs
    (let ((ex (gensym "OR")))
      `(let ((,ex ,(car exprs)))
         (if ,ex ,ex (or ,@(cdr exprs)))))))

(def-emac and (&rest exprs)
  (if exprs
      (let ((ex (gensym "AND")))
        `(let ((,ex ,(car exprs)))
           (when ,ex
             ,(if (cdr exprs) `(and ,@(cdr exprs)) ex))))
      t))

(def-efun member (item list &optional (test (function eq)))
  (if list
      (if (funcall test item (car list))
          list
          (member item (cdr list)))))

(def-emac case (expr &rest cases)
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

(def-efun foreach (list func)
  (when list
    (funcall func (car list))
    (foreach (cdr list) func)
    nil))

(def-efun reduce (list proc init)
  (if list
      (reduce (cdr list) proc (funcall proc (car list) init))
      init))

(def-efun reverse (list)
  (reduce list (function cons) nil))

(def-efun length (list)
  (labels ((rec (list len)
             (if list
                 (rec (cdr list) (1+ len))
                 len)))
    (rec list 0)))

(def-efun map (func list)
  (if list
      (cons (funcall func (car list))
            (map func (cdr list)))))

 ;;; END

(in-package :cl-user)
t
