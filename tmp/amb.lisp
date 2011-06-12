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

(let ((n 10)
      (required 1))
  (labels ((rec (numbers next)
             (if (= next n)
                 (progn
                   (if (not (= required (apply (function +) next numbers)))
                       ;; not required sum, then fail
                       (amb)
                       ;; solution:
                       (progn
                         (print (cons next numbers))
                         (amb))))
                 (rec (cons (amb next (- next))
                            (copy-list numbers))
                      (1+ next)))))
    (rec nil 1)))
