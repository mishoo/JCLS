(jcls:print "Entering evaluator")

(flet ((q ()
         `(1 2 3)))
  (jcls:print (eq (q) (q))))

(let ((x 0))
  (defun counter ()
    (setq x (1+ x))))

(defmacro not (condition)
  `(if ,condition nil t))

(defmacro when (condition &body body)
  `(if ,condition
       (progn ,@body)))

(defmacro unless (condition &body body)
  `(when (not ,condition)
     ,@body))

(defmacro cond (cases)
  (if cases
      `(if ,(caar cases)
           (progn ,@(cdar cases))
           (cond ,(cdr cases)))))

(defmacro while (condition &body body)
  (let ((repeat (gensym "REPEAT")))
    `(labels ((,repeat ()
                (when ,condition
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

(let ((a 'G6)                           ; G6 is the next
      (b (gensym)))
  (jcls:print a b (if (eq a b) "same symbol" "not same symbol")))

(defun map (list proc)
  (when list
    (cons (funcall proc (car list))
          (map (cdr list) proc))))

(defun foreach (list proc)
  (when list
    (funcall proc (car list))
    (foreach (cdr list) proc)))

(defun append (a b)
  (if a
      (cons (car a) (append (cdr a) b))
      b))

(defun reduce (list proc init)
  (if list
      (reduce (cdr list) proc (funcall proc (car list) init))
      init))

(defun reverse (list)
    (reduce list (function cons) nil))

(set-macro-character "]" (get-macro-character ")"))
(set-macro-character "[" (lambda (stream ch)
                           (reverse (read-delimited-list "]" stream))))

;; now square brackets delimit lists in reverse order
[[2 1 cons] jcls:print]
(jcls:print (read-from-string "[[2 1 cons] jcls:print]"))

(jcls:print (catch 'foo
              (let ((x 10))
                (throw 'foo 'aborted)
                (* x x))))

(jcls:print (unwind-protect
                 (progn
                   (jcls:print "Got here")
                   1)
              (jcls:print "And here too")
              2))

(defvar *test* 10)
(catch 'foo
  (let ((*test* 20))
    (throw 'foo nil)))
(jcls:print *test*)

(defmacro and (&rest exprs)
  (if exprs
      `(when ,(car exprs)
         (and ,@(cdr exprs)))
      t))

(defmacro or (&rest exprs)
  (when exprs
    `(unless ,(car exprs)
       (or ,@(cdr exprs)))))

(defun member (item list)
  (if list
      (if (eq item (car list))
          list
          (member item (cdr list)))))

(jcls:print (member 'foo '(1 2 bar mak foo test şmen)))
(jcls:print (member 'foo '(1 2 3)))

(defmacro zcase (expr &rest cases)
  (let ((vexpr (gensym "ZCASE")))
    `(let ((,vexpr ,expr))
       ,(labels ((recur (cases)
                        (when cases
                          (if (consp (caar cases))
                              `(if (member ,vexpr ',(caar cases))
                                   (progn ,@(cdar cases))
                                   ,(recur (cdr cases)))
                              `(if (eq ,vexpr ',(caar cases))
                                   (progn ,@(cdar cases))
                                   ,(recur (cdr cases)))))))
                (recur cases)))))

(jcls:print "CASE test")
(jcls:print (zcase 'mak ((bar) 2) ((mak) 'crap) ((foo) 1)))

;; ---------------------------------------------------------------------
;; "native" functions/methods/data -- from the underlying JS environment
;;

(jcls:print (let ((obj (funcall (jcls:native "Array") 1 2 3)))
              (jcls:call-native "join" obj '(", "))))
(jcls:print (jcls:call-native '("Math" "floor") nil '(3.14)))

(defmacro defnative-var (name &rest path)
  `(defparameter ,name (jcls:native ,@path)))

(defmacro defnative-func (name &rest path)
  `(defun ,name (&rest args)
     (jcls:call-native ',path nil args)))

(defnative-var pi "Math" "PI")
(jcls:print pi)

(defnative-func random "Math" "random")
(jcls:print (random))

(defnative-func set-timeout "setTimeout")
(defnative-func clear-timeout "clearTimeout")
(defnative-func set-interval "setInterval")
(defnative-func clear-interval "clearInterval")

(defmacro with-timeout (time &body body)
  `(set-timeout (jcls:to-native (lambda () ,@body)) ,time))

(defmacro with-interval (time &body body)
  `(set-interval (jcls:to-native (lambda () ,@body)) ,time))

(with-timeout 1000
  (jcls:print "This is printed after 1000ms"))

(defmacro every (timeout unit &body body)
  (with-interval (* timeout (case unit
                              ((seconds sec s) 1000)
                              ((milliseconds ms) 1)))))

(let* ((calls 10)
       (timer (with-interval 150
                (if (= 0 (setq calls (1- calls)))
                    (clear-interval timer))
                (jcls:print "From interval: " calls)))))
