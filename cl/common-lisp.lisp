(jcls:set! *package* "v" (jcls:find-package "JCLS"))

(defmacro def-f! (name args &body body)
  `(def! ,name "f" (lambda ,args ,@body)))

(defmacro in-package (name)
  `(set! *package* "v" (find-package name)))

(in-package :cl)

(jcls:import '(jcls::fork-environment
               jcls::def!
               jcls::set!
               jcls::def-f!
               jcls::in-package
               jcls::export
               jcls::special!
               jcls::special?))

(defmacro def-emac (name args &body body)
  `(progn
     (export ',name)
     (defmacro ,name ,args ,@body)))

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
   ;; XXX: this is quite inefficient (at compile-time)
   (def-f! recur (defs)
     (if defs
         (progn
           (set! names "v" `(,@names ,(caar defs)))
           (set! values "v" `(,@values ,(cadar defs)))
           (recur (cdr defs)))))
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
  (labels ((recur (defs)
             (if defs
                 (cons `(list ',(caar defs) (lambda ,(cadar defs) ,@(cddar defs)))
                       (recur (cdr defs))))))
    (let ((tmp (gensym)))
      `(let ((,tmp (list ,@(recur defs))))
         (foreach ,tmp
                  (lambda (def)
                    (def! (car def) "f" (cadr def))))
         ,@body))))

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

(def-efun copy-list (list)
  (if list (cons (car list)
                 (copy-list (cdr list)))))

(labels ((a2 (a b)
           (if a
               (cons (car a) (a2 (cdr a) b))
               b)))
  (def-efun append (&rest seqs)
    (if (cdr seqs)
        (a2 (car seqs) (apply (function append) (cdr seqs)))
        (if (listp (car seqs))
            (copy-list (car seqs))
            (car seqs)))))

(def-efun foreach (list func)
  (if list
      (progn
        (funcall func (car list))
        (foreach (cdr list) func)
        nil)))

 ;;; END

(in-package :cl-user)
t
