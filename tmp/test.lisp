(jcls:print "Entering evaluator")

(let* ((a '(1 2 3 4))
       (b '(1 2 3 4))
       (c `(,@a foo ,@a bar ,.b baz)))
  (jcls:print c)
  (jcls:print b)
  (jcls:print a))

(let ((a '(1 2)))
  (jcls:print `(,.a ,@a))
  (jcls:print a)
  ;; but the following makes a cycle and our print function loops forever
  ;; (jcls:print `(,.a ,.a))
  )

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

(defun length (list)
  (labels ((rec (list len)
             (if list
                 (rec (cdr list) (1+ len))
                 len)))
    (rec list 0)))

(defun nth (n list)
  (if (= n 0)
      (car list)
      (nth (1- n) (cdr list))))

(jcls:print (length (list 1 2 3 4)))

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
      (let ((ex (gensym "AND")))
        `(let ((,ex ,(car exprs)))
           (when ,ex
             ,(if (cdr exprs) `(and ,@(cdr exprs)) ex))))
      t))

(jcls:print "AND: " (and 1 2 3 4))
(jcls:print "AND: " (and 1 2 nil 4))
(jcls:print "AND: " (and 4))
(jcls:print "AND: " (and))

(defmacro or (&rest exprs)
  (when exprs
    (let ((ex (gensym "OR")))
      `(let ((,ex ,(car exprs)))
         (if ,ex ,ex (or ,@(cdr exprs)))))))

(jcls:print "OR: " (or 1 2 3 4))

(defun member (item list)
  (if list
      (if (eq item (car list))
          list
          (member item (cdr list)))))

(jcls:print (member 'foo '(1 2 bar mak foo test şmen)))
(jcls:print (member 'foo '(1 2 3)))

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
(defnative-func floor "Math" "floor")
(jcls:print (random))

(defnative-func set-timeout "setTimeout")
(defnative-func clear-timeout "clearTimeout")
(defnative-func set-interval "setInterval")
(defnative-func clear-interval "clearInterval")

(defmacro with-timeout (time &body body)
  `(set-timeout (jcls:to-native (lambda () ,@body)) ,time))

(defmacro with-interval (time &body body)
  `(set-interval (jcls:to-native (lambda () ,@body)) ,time))

(with-timeout 333
  (jcls:print "This is printed after 333ms"))

(let* ((calls 10)
       (timer (with-interval 100
                (if (= 0 (setq calls (1- calls)))
                    (clear-interval timer))
                (jcls:print "From interval 1: " calls)))))

;; ---------------------------------------------------------------------

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
                (let ((ret (recur cases)))
                  (jcls:print ret)
                  ret)))))

(let* ((list '(bar mak 1 2 3 4 foo else))
       (el (nth (floor (* (length list) (random))) list)))
  (jcls:print "CASE test" el)
  (jcls:print (case el
                (bar
                 (jcls:print "- first case")
                 2)
                ((mak)
                 (jcls:print "- second case")
                 'crap)
                ((1 2 3 4)
                 (jcls:print "- third case")
                 'foo)
                (foo
                 (jcls:print "- fourth case")
                 (+ 2 2))
                (otherwise "None of the above"))))

(defmacro every (timeout unit &body body)
  (let ((timer (gensym)))
    `(let (,timer)
       (flet ((stop () (clear-interval ,timer)))
         (setq ,timer
               (with-interval ,(* timeout (case unit
                                            ((seconds sec s) 1000)
                                            ((milliseconds ms) 1)))
                 ,@body))))))

(let ((calls 10))
  (every 0.05 seconds
         (if (= 0 (setq calls (1- calls)))
             (stop))
         (jcls:print "From interval 2: " calls)))
