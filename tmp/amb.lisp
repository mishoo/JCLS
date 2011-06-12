(jcls:import '(jcls:print jcls:call/cc))

;; http://www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme-Z-H-16.html

(defparameter amb-fail
  (lambda ()
    (print "amb tree exhausted")))

(defun map (func list)
  (if list
      (cons (funcall func (car list))
            (map func (cdr list)))))

(defmacro amb (&rest alternatives)
  `(let ((+prev-amb-fail amb-fail))
     (call/cc
      (lambda (+sk)
        ,@(map (lambda (alt)
                 `(call/cc
                   (lambda (+fk)
                     (setq amb-fail
                           (lambda ()
                             (setq amb-fail +prev-amb-fail)
                             (funcall +fk 'fail)))
                     (funcall +sk ,alt))))
               alternatives)
        (funcall +prev-amb-fail)))))

;; (print (macroexpand-1 '(amb 1 2)))

;; (let ((a (amb 1 2 3 4 5 6))
;;       (b (amb 1 2 3 4 5 6)))
;;   (unless (= 12 (* a b))
;;     (amb))
;;   (print "a: " a "b: " b)
;;   (amb))

;; (print "Okay, let's see more.")

(defun solutions (n &optional (required 1))
  (call/cc
   (lambda (k)
     (setq amb-fail k)
     (labels ((required-sum? (numbers)
                (= required (apply (function +) numbers)))
              (rec (numbers next)
                (if (= next 0)
                    (progn
                      (when (required-sum? numbers)
                        (print numbers))
                      (amb))
                    (rec (cons (amb next (- next))
                               (copy-list numbers))
                         (1- next)))))
       (rec nil n)))))

(solutions 9)
(print "we're done")
