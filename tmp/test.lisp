;; foo
;; bar

(progn
  (defun test (x)
    `(1 ,@x ,(+ 2 3) ,@'(a b c d)
        (foo (bar ,@x))
        (mak `(1 2 ,,@x ,,(+ 1 2)))
        ,@x))
  (test (list 'crap 'mak))
  )

(progn

  (defun map (list proc)
    (if list
        (cons (funcall proc (car list))
              (map (cdr list) proc))))

  (defun foreach (list proc)
    (if list
        (progn
          (funcall proc (car list))
          (foreach (cdr list) proc))))

  (defun reduce (list proc init)
    (if list
        (reduce (cdr list) proc (funcall proc (car list) init))
        init))

  (defun append (a b)
    (if a
        (cons (car a) (append (cdr a) b))
        b))

  ;; quite inefficient
  (defun reverse (list)
    (if list
        (append (reverse (cdr list)) (cons (car list) nil))))

  ;; this is a LOT faster
  (defun reverse (list)
    (let ((ret nil))
      (foreach list (lambda (el)
                      (setq ret (cons el ret))))
      ret))

  (defun reverse (list)
    (reduce list (function cons) nil))

  ;; (io:log (reverse '(1 2 3)))

  (defun range (x)
    (if (> x 0)
        (cons x (range (- x 1)))))

  ;; (reverse (range 400))

  (defun sqr (x) (* x x))

  (map (reverse (range 1000)) (function sqr))

  ;; test comment

  )





(progn
  (let ((tmp 0))
    (defun count ()
      (setq tmp (+ tmp 1))))
  (io:log (count))
  (io:log (count))
  (io:log (count))
  (io:log (count)))

(let ((x 10))
  (setq x 20)
  x)

(labels ((fact (n)
           (if (= n 1) 1
               (* n (fact (- n 1))))))
  (fact 10))

(let ((x 10))
  (let* ((y x)
         (x (+ x x))
         (z (* x 5)))
    (io:log (+ x y z)))
  ;;foo
  x)

(progn
  (let ((tmp 0))
    (defun count ()
      (setq tmp (+ tmp 1))))
  (io:log (count))
  (io:log (count))
  (io:log (count))
  (io:log (count)))

(progn
  (defun sqr (x)
    (* x x))
  (sqr 8))

(labels ((fact (n)
           (if (= n 1) 1
               (* n (fact (- n 1))))))
  (fact 10))

(progn
  (io:log ((lambda (f)
             (funcall f f 10))
           (lambda (f n)
             (if (= n 1) 1
                 (* n (funcall f f (- n 1)))))))
  (io:log "Check this out"))
