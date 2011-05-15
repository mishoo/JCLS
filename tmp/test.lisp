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

(let ((a 'G6)                           ; G6 is the next
      (b (gensym)))
  (jcls:print a b (if (eq a b) "same symbol" "not same symbol")))

(defun map (list proc)
  (if list
      (cons (funcall proc (car list))
            (map (cdr list) proc))))

(defun foreach (list proc)
  (if list
      (progn
        (funcall proc (car list))
        (foreach (cdr list) proc))))

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
(defmacro with-timeout (time &body body)
  `(set-timeout (jcls:to-native (lambda () ,@body)) ,time))

(with-timeout 1000
  (jcls:print "This is printed after 1000ms"))
