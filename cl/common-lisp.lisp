(jcls:set! *package* "v" (jcls:find-package "JCLS"))

(defmacro def-f! (name args &body body)
  `(def! ,name "f" (lambda ,args ,@body)))

(defmacro def-v! (name val)
  `(def! ,name "v" ,val))

(defmacro set-v! (name val)
  `(set! ,name "v" ,val))

(defmacro in-package (name)
  `(set! *package* "v" (find-package name)))

(in-package :cl)

(jcls:import '(jcls::fork-environment
               jcls::def-f!
               jcls::def-v!
               jcls::set-v!
               jcls::in-package
               jcls::export))

(defmacro def-emac (name args &body body)
  `(progn
     (export ',name)
     (defmacro ,name ,args ,@body)))

(def-emac setq (&rest defs)
  (fork-environment
   (def-f! recur (defs)
     (if defs
         `((set-v! ,(car defs) ,(cadr defs))
           ,@(recur (cddr defs)))))
   `(progn ,@(recur defs))))

(def-emac let* (defs &body body)
  (fork-environment
   (def-f! recur (defs)
     (if defs
         `((def-v! ,(caar defs) ,(cadar defs))
           ,@(recur (cdr defs)))))
   `(fork-environment
     ,@(recur defs)
     ,@body)))

(def-emac let (defs &body body)
  (fork-environment
   (def-v! names nil)
   (def-v! values nil)
   ;; XXX: this is quite inefficient
   (def-f! recur (defs)
     (if defs
         (progn
           (set-v! names `(,@names ,(caar defs)))
           (set-v! values `(,@values ,(cadar defs)))
           (recur (cdr defs)))))
   (recur defs)
   `((lambda ,names ,@body) ,@values)))

(def-emac labels (defs &body body)
  (fork-environment
   (def-f! recur (defs)
     (if defs
         `((def-f! ,(caar defs) ,(cadar defs) ,@(cddar defs))
           ,@(recur (cdr defs)))))
   `(fork-environment
     ,@(recur defs)
     ,@body)))

(def-emac defun (name args &body body)
  `(def-f! ,name ,args ,@body))

(defmacro def-efun (name args &body body)
  `(progn
     (export ',name)
     (defun ,name ,args ,@body)))



(def-emac not (condition)
  `(if ,condition nil t))

(def-emac when (condition &body body)
  `(if ,condition
       (progn ,@body)))

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

(def-efun append (&rest lists)
  )

 ;;; END

(in-package :cl-user)
t
