(jcls:import '(jcls:print jcls:call/cc jcls:with-cc))

;; http://www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme-Z-H-16.html

(defparameter amb-fail
  (lambda ()
    (print "amb tree exhausted")))

(defun map (func list)
  (if list
      (cons (funcall func (car list))
            (map func (cdr list)))))

(defmacro amb (&rest alternatives)
  (if alternatives
      `(let ((+prev-amb-fail amb-fail))
         (with-cc (+sk)
           ,@(map (lambda (alt)
                    `(with-cc (+fk)
                       (setq amb-fail +fk)
                       (funcall +sk ,alt)))
                  alternatives)
           (setq amb-fail +prev-amb-fail)
           (funcall +prev-amb-fail)))
      `(funcall amb-fail)))

;; (print (macroexpand-1 '(amb 1 2)))

;; (call/cc
;;  (lambda (k)
;;    (setq amb-fail k)
;;    (let ((a (amb 1 2 3 4 5 6))
;;          (b (amb 1 2 3 4 5 6)))
;;      (unless (= 12 (* a b))
;;        (amb))
;;      (print "a: " a "b: " b)
;;      (amb))))

;; (print "Okay, let's see more.")

(defun solutions (n &optional (sum 1))
  (with-cc (k)
    (setq amb-fail k)
    (labels ((required-sum? (numbers)
               (= sum (apply (function +) numbers)))
             (rec (numbers next)
               (if (= next 0)
                   (progn
                     (when (required-sum? numbers)
                       (print numbers))
                     (amb))
                   (rec (cons (amb next (- next)) numbers)
                        (1- next)))))
      (rec () n))))

(solutions 10)
(print "we're done")
