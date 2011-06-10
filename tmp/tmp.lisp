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

(export 'setq)
(defmacro setq (&rest defs)
  (fork-environment
   (def-f! recur (defs)
     (if defs
         `((set-v! ,(car defs) ,(cadr defs))
           ,@(recur (cddr defs)))))
   `(progn ,@(recur defs))))

(export 'let*)
(defmacro let* (defs &body body)
  (fork-environment
   (def-f! recur (defs)
     (if defs
         `((def-v! ,(caar defs) ,(cadar defs))
           ,@(recur (cdr defs)))))
   `(fork-environment
     ,@(recur defs)
     ,@body)))

(export 'let)
(defmacro let (defs &body body)
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

(export 'labels)
(defmacro labels (defs &body body)
  (fork-environment
   (def-f! recur (defs)
     (if defs
         `((def-f! ,(caar defs) ,(cadar defs) ,@(cddar defs))
           ,@(recur (cdr defs)))))
   `(fork-environment
     ,@(recur defs)
     ,@body)))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(let* ((a 5)
       (b (+ a 3))
       (c (* a b)))
  (jcls:print a b c)
  (setq c 1234)
  (jcls:print c))

(let ((foo 5))
  (let ((foo 1)
        (bar (+ foo 1)))
    (+ foo bar)))

(labels ((add (x y) (+ x y))
         (mul (x y) (* x y))
         (test (x y) (add (mul x y) 1)))
  (jcls:print (add 10 (mul 2 3)))
  (jcls:print (test 2 3)))

(labels ((sum (n)
           (if (= n 0) 0
               (+ n (sum (1- n))))))
  (jcls:print (sum 10000)))

(gensym)

