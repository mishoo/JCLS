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

[[2 1 list] jcls:print]
